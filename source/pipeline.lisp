;;;; -- Pipelines and sequences --
;;;
;;; Lisp helpers over shell commands: PIPE connects stages with Unix
;;; style pipes, SEQ runs stages one after another, ALL and ANY are the
;;; && and || equivalents, and CAPTURE returns a pipeline's output as a
;;; string. A stage is written (name argument...) where NAME resolves
;;; exactly like the first word of a command line. Arguments are evaluated
;;; Lisp expressions; proper lists splice into the argument vector, which
;;; lets the public GLOB function supply any number of matching paths.
;;;
;;; Redirection is spelled as stages recognized by name inside PIPE and
;;; CAPTURE: (from "file") as the first stage feeds standard input,
;;; (to "file") or (append-to "file") as the last stage writes it, and
;;; anywhere in the pipeline (error-to "file"), (error-append-to
;;; "file") or (merge-error) direct the standard error of every stage
;;; to a file or into the ordinary output.
;;;
;;; Stages are connected with close-on-exec kernel pipes. Lisp builtin
;;; workers wrap their pipe descriptors as UTF-8 streams, while external
;;; programs receive the descriptors directly through POSIX spawn.

(in-package #:cclsh)

(define-condition pipeline-syntax-error (shell-error)
  ((message :initarg :message :reader pipeline-syntax-error-message))
  (:documentation "Signaled for malformed pipeline redirections.")
  (:report (lambda (condition stream)
             (format stream "~a" (pipeline-syntax-error-message condition)))))

(defun pipeline--argument-words (value)
  "Convert one evaluated stage argument VALUE into command words.

NIL contributes no words, a proper list contributes one word per element,
and every other value contributes one word. Elements and scalar values use
PRINC-TO-STRING, preserving the former scalar argument behavior."
  (cond ((null value)
         nil)
        ((and (listp value)
              (ignore-errors (list-length value)))
         (mapcar #'princ-to-string value))
        (t
         (list (princ-to-string value)))))

(defun pipeline--stage-form (stage)
  "Translate one (name argument...) STAGE into a runtime stage form."
  (destructuring-bind (head &rest arguments) stage
    (let ((name (command-designator-name head)))
      (if arguments
          `(cons ,name
                 (append
                  ,@(mapcar (lambda (argument)
                              `(pipeline--argument-words ,argument))
                            arguments)))
          `(list ,name)))))

(defmacro pipe (&rest stages)
  "Run STAGES as a pipeline: (pipe (ls \"-la\") (grep \"lisp\")).
   Proper-list arguments splice, so (glob \"*.lisp\") supplies every match.
   (from \"file\") first feeds standard input, (to \"file\") or
   (append-to \"file\") last redirects the output, and (error-to
   \"file\"), (error-append-to \"file\") or (merge-error) direct
   standard error. Returns the exit status of the last command stage."
  `(pipeline-run (list ,@(mapcar #'pipeline--stage-form stages))))

(defmacro capture (&rest stages)
  "Run STAGES as a pipeline and return its standard output as a string
   with trailing newlines removed, so (capture (git \"rev-parse\"
   \"HEAD\")) is sh's $(git rev-parse HEAD). Proper-list arguments splice.
   Returns (values string status) and records *LAST-STATUS*."
  `(pipeline-capture (list ,@(mapcar #'pipeline--stage-form stages))))

(defmacro cmd (name &rest arguments)
  "Run one command from the middle of Lisp code: (cmd git \"status\").
   NAME resolves like the first word of a command line and ARGUMENTS
   are evaluated Lisp expressions, stringified like pipe stages; proper
   lists splice into the argument vector. Returns the exit status."
  `(stage-sequence-run (list ,(pipeline--stage-form (list* name arguments)))
                       ':always))

(defmacro seq (&rest stages)
  "Run STAGES in order regardless of exit statuses, like cmd1; cmd2.
   Returns the exit status of the last stage."
  `(stage-sequence-run (list ,@(mapcar #'pipeline--stage-form stages))
                       ':always))

(defmacro all (&rest stages)
  "Run STAGES in order, stopping at the first failure, like cmd1 && cmd2.
   Returns the first nonzero exit status, or zero."
  `(stage-sequence-run (list ,@(mapcar #'pipeline--stage-form stages))
                       ':while-successful))

(defmacro any (&rest stages)
  "Run STAGES in order, stopping at the first success, like cmd1 || cmd2.
   Returns the first zero exit status, or the last status."
  `(stage-sequence-run (list ,@(mapcar #'pipeline--stage-form stages))
                       ':until-successful))


;;; Runtime

(defstruct pipeline-stage
  "One validated command stage with its three prepared descriptors."
  kind
  target
  arguments
  label
  input-fd
  output-fd
  error-fd
  output-pipe-p
  input-demand-fd
  task)

(defstruct pipeline-plan
  "A fully opened pipeline which has not spawned any children yet."
  stages
  descriptors
  capture-read-fd
  sentinel-path
  sentinel-read-fd
  sentinel-null-fd
  sentinel-write-fd
  tty-proxy-path
  tty-proxy-demand-read-fd
  tty-proxy-write-fd
  command)

(defstruct (pipeline-gate (:constructor pipeline--make-gate ()))
  "A one-shot gate which holds builtin side effects until attendance."
  (semaphore (ccl:make-semaphore))
  (lock (ccl:make-lock "cclsh pipeline gate"))
  (open nil)
  (cancelled nil))

(defstruct pipeline-task
  "One UTF-8 Lisp worker or capture collector."
  name
  function
  gate
  owners
  expected-broken-pipe
  thread
  (done nil)
  (state ':running)
  code
  value
  error
  (joined nil)
  group)

(defstruct pipeline-task-group
  "Shared lifecycle state for the Lisp and external sides of a job."
  job
  gate
  tasks
  first-task
  anchor-process
  tty-proxy
  control-fd
  processes
  process-group
  (lock (ccl:make-lock "cclsh pipeline tasks"))
  (suspended nil)
  (aborted nil)
  abort-signal)

(defstruct pipeline-run-context
  "Dynamic external execution context for one builtin pipeline stage."
  group
  input-fd
  output-fd
  error-fd
  close-fds
  input-demand)

(defvar *pipeline-task-starter* #'ccl:process-run-function
  "Function used to start Lisp pipeline workers. Tests may bind it to
   inject a partial thread-start failure.")

(defclass pipeline-demand-input-stream
    (#+ccl ccl:fundamental-character-input-stream
     #+sbcl trivial-gray-streams:fundamental-character-input-stream)
  ((input
    :initarg :input
    :reader pipeline-demand-input)
   (demand-output
    :initarg :demand-output
    :accessor pipeline-demand-output)
   (lock
    :initform (ccl:make-lock "cclsh terminal input demand")
    :reader pipeline-demand-lock))
  (:documentation
   "A builtin input stream which starts its tty proxy on first use."))

(defun pipeline--demand-input-start (stream)
  "Wake STREAM's tty proxy exactly once before its first input call."
  (let ((output nil))
    (ccl:with-lock-grabbed ((pipeline-demand-lock stream))
      (setf output (shiftf (pipeline-demand-output stream) nil)))
    (when output
      (write-char #\newline output)
      (force-output output)
      (close output)))
  (values))

#+ccl
(defmethod ccl:stream-read-char ((stream pipeline-demand-input-stream))
  (pipeline--demand-input-start stream)
  (read-char (pipeline-demand-input stream) nil ':eof))

#+sbcl
(defmethod trivial-gray-streams:stream-read-char
    ((stream pipeline-demand-input-stream))
  (pipeline--demand-input-start stream)
  (read-char (pipeline-demand-input stream) nil ':eof))

#+ccl
(defmethod ccl:stream-read-char-no-hang
    ((stream pipeline-demand-input-stream))
  (pipeline--demand-input-start stream)
  (read-char-no-hang (pipeline-demand-input stream) nil ':eof))

#+sbcl
(defmethod trivial-gray-streams:stream-read-char-no-hang
    ((stream pipeline-demand-input-stream))
  (pipeline--demand-input-start stream)
  (read-char-no-hang (pipeline-demand-input stream) nil ':eof))

#+ccl
(defmethod ccl:stream-unread-char
    ((stream pipeline-demand-input-stream) character)
  (unread-char character (pipeline-demand-input stream)))

#+sbcl
(defmethod trivial-gray-streams:stream-unread-char
    ((stream pipeline-demand-input-stream) character)
  (unread-char character (pipeline-demand-input stream)))

#+ccl
(defmethod ccl:stream-listen ((stream pipeline-demand-input-stream))
  (pipeline--demand-input-start stream)
  (listen (pipeline-demand-input stream)))

#+sbcl
(defmethod trivial-gray-streams:stream-listen
    ((stream pipeline-demand-input-stream))
  (pipeline--demand-input-start stream)
  (listen (pipeline-demand-input stream)))

#+ccl
(defmethod ccl:stream-clear-input ((stream pipeline-demand-input-stream))
  (pipeline--demand-input-start stream)
  (clear-input (pipeline-demand-input stream)))

#+sbcl
(defmethod trivial-gray-streams:stream-clear-input
    ((stream pipeline-demand-input-stream))
  (pipeline--demand-input-start stream)
  (clear-input (pipeline-demand-input stream)))

#+ccl
(defmethod ccl:stream-read-line ((stream pipeline-demand-input-stream))
  (pipeline--demand-input-start stream)
  (multiple-value-bind (line missing-newline)
      (read-line (pipeline-demand-input stream) nil nil)
    (values line missing-newline)))

#+sbcl
(defmethod trivial-gray-streams:stream-read-line
    ((stream pipeline-demand-input-stream))
  (pipeline--demand-input-start stream)
  (multiple-value-bind (line missing-newline)
      (read-line (pipeline-demand-input stream) nil nil)
    (values line missing-newline)))


;;; Validation and planning

(defun pipeline--redirect-path (name arguments)
  "Validate and return NAME's sole nonempty path argument."
  (unless (= (length arguments) 1)
    (error 'pipeline-syntax-error
           :message (format nil "~a takes exactly one file path" name)))
  (let ((path (first arguments)))
    (when (or (not (stringp path)) (zerop (length path)))
      (error 'pipeline-syntax-error
             :message (format nil "~a needs a file path" name)))
    path))

(defun pipeline--resolve-stages (stages)
  "Resolve and validate every command and redirect before opening files."
  (loop for (name . arguments) in stages
        collect
        (cond ((string= name "from")
               (list ':redirect-in
                     (pipeline--redirect-path name arguments)
                     nil))
              ((string= name "to")
               (list ':redirect-out
                     (pipeline--redirect-path name arguments)
                     nil))
              ((string= name "append-to")
               (list ':redirect-out
                     (pipeline--redirect-path name arguments)
                     t))
              ((string= name "error-to")
               (list ':redirect-error
                     (pipeline--redirect-path name arguments)
                     nil))
              ((string= name "error-append-to")
               (list ':redirect-error
                     (pipeline--redirect-path name arguments)
                     t))
              ((string= name "merge-error")
               (when arguments
                 (error 'pipeline-syntax-error
                        :message "merge-error takes no arguments"))
               (list ':merge-error nil nil))
              (t
               (multiple-value-bind (kind target)
                   (command-resolve-fresh name)
                 (when (eq kind ':unknown)
                   (error 'command-not-found-error :name name))
                 (list kind target arguments
                       (format nil "~{~a~^ ~}"
                               (cons name arguments))))))))

(defun pipeline--split-redirects (resolved)
  "Return validated commands and the six global redirect settings."
  (let ((error-path nil)
        (error-append nil)
        (merge-error nil)
        (remaining nil))
    (dolist (entry resolved)
      (case (first entry)
        (:redirect-error
         (when (or error-path merge-error)
           (error 'pipeline-syntax-error
                  :message
                  "error redirections are mutually exclusive"))
         (setf error-path (second entry)
               error-append (third entry)))
        (:merge-error
         (when (or error-path merge-error)
           (error 'pipeline-syntax-error
                  :message
                  "error redirections are mutually exclusive"))
         (setf merge-error t))
        (t
         (push entry remaining))))
    (let ((commands (nreverse remaining))
          (input-path nil)
          (output-path nil)
          (append nil))
      (when (and commands
                 (eq (first (first commands)) ':redirect-in))
        (setf input-path (second (first commands))
              commands (rest commands)))
      (let ((final (first (last commands))))
        (when (and final (eq (first final) ':redirect-out))
          (setf output-path (second final)
                append (third final)
                commands (butlast commands))))
      (when (find-if (lambda (entry)
                       (member (first entry)
                               '(:redirect-in :redirect-out)))
                     commands)
        (error 'pipeline-syntax-error
               :message
               "from must be first; to or append-to must be last"))
      (values commands input-path output-path append
              error-path error-append merge-error))))

(defun pipeline--sentinel-path ()
  "Return the external CAT used to anchor jobs containing builtins."
  (or (and (probe-file #p"/usr/bin/cat") "/usr/bin/cat")
      (and (probe-file #p"/bin/cat") "/bin/cat")
      (multiple-value-bind (kind target)
          (command-resolve-fresh "cat")
        (and (eq kind ':external) target))
      (error "cclsh cannot find cat for builtin pipeline job control")))

(defun pipeline--proxy-shell-path ()
  "Return the POSIX shell used by the lazy terminal-input proxy."
  (or (and (probe-file #p"/bin/sh") "/bin/sh")
      (and (probe-file #p"/usr/bin/sh") "/usr/bin/sh")
      (error "cclsh cannot find sh for builtin terminal input")))

(defun pipeline--close-plan (plan)
  "Close every original descriptor still owned by PLAN exactly once."
  (dolist (descriptor (shiftf (pipeline-plan-descriptors plan) nil))
    (fd-close descriptor))
  (values))

(defun pipeline--prepare (resolved &key capture)
  "Open every redirect and pipe, then return an unspawned plan."
  (multiple-value-bind (commands input-path output-path append
                        error-path error-append merge-error)
      (pipeline--split-redirects resolved)
    (when (and capture output-path)
      (error 'pipeline-syntax-error
             :message "capture cannot combine with to or append-to"))
    (when (null commands)
      (unless (and input-path (or output-path capture))
        (error 'pipeline-syntax-error
               :message "a pipeline needs at least one command"))
      (setf commands
            (list (list ':external
                        (pipeline--sentinel-path)
                        nil
                        (format nil "copy ~a" input-path)))))
    (let* ((count (length commands))
           (last-index (1- count))
           (builtin-p (find ':builtin commands :key #'first))
           (tty-proxy-p (and builtin-p
                             (eq (first (first commands)) ':builtin)
                             (null input-path)
                             (terminal-tty-p)))
           (helper-path (and builtin-p (pipeline--sentinel-path)))
           (proxy-path (and tty-proxy-p
                            (pipeline--proxy-shell-path)))
           (descriptors nil)
           (complete nil)
           (plan nil))
      (labels ((remember (descriptor)
                 (push descriptor descriptors)
                 descriptor)
               (new-pipe ()
                 (multiple-value-bind (read-fd write-fd)
                     (fd-cloexec-pipe)
                   (values (remember read-fd)
                           (remember write-fd)))))
        (unwind-protect
            (let ((input-fd (and input-path
                                 (remember (fd-open-input input-path))))
                  (output-fd (and output-path
                                  (remember
                                   (fd-open-output output-path
                                                   :append append))))
                  (error-fd (and error-path
                                 (remember
                                  (fd-open-output error-path
                                                  :append error-append))))
                  (boundaries (make-array (max 0 (1- count))))
                  (capture-read nil)
                  (capture-write nil)
                  (sentinel-read nil)
                  (sentinel-write nil)
                  (sentinel-null nil)
                  (proxy-read nil)
                  (proxy-write nil)
                  (proxy-demand-read nil)
                  (proxy-demand-write nil))
              (dotimes (index (length boundaries))
                (multiple-value-bind (read-fd write-fd)
                    (new-pipe)
                  (setf (aref boundaries index)
                        (cons read-fd write-fd))))
              (when capture
                (multiple-value-setq (capture-read capture-write)
                  (new-pipe)))
              (when builtin-p
                (multiple-value-setq (sentinel-read sentinel-write)
                  (new-pipe))
                (setf sentinel-null
                      (remember (fd-open-output "/dev/null"))))
              (when tty-proxy-p
                (multiple-value-setq (proxy-read proxy-write)
                  (new-pipe))
                (multiple-value-setq
                    (proxy-demand-read proxy-demand-write)
                  (new-pipe)))
              (let ((stages
                      (loop for command in commands
                            for index from 0
                            for output-pipe-p = (or (< index last-index)
                                                    capture)
                            for fd0 = (cond ((and (zerop index)
                                                  tty-proxy-p)
                                             proxy-read)
                                            ((zerop index)
                                             (or input-fd 0))
                                            (t
                                             (car (aref boundaries
                                                        (1- index)))))
                            for fd1 = (cond ((< index last-index)
                                             (cdr (aref boundaries index)))
                                            (capture capture-write)
                                            (output-fd output-fd)
                                            (t 1))
                            for fd2 = (cond (merge-error fd1)
                                            (error-fd error-fd)
                                            (t 2))
                            collect
                            (make-pipeline-stage
                             :kind (first command)
                             :target (second command)
                             :arguments (third command)
                             :label (fourth command)
                             :input-fd fd0
                             :output-fd fd1
                             :error-fd fd2
                             :input-demand-fd
                             (and (zerop index)
                                  proxy-demand-write)
                             :output-pipe-p output-pipe-p))))
                (setf plan
                      (make-pipeline-plan
                       :stages stages
                       :descriptors (nreverse descriptors)
                       :capture-read-fd capture-read
                       :sentinel-path helper-path
                       :sentinel-read-fd sentinel-read
                       :sentinel-null-fd sentinel-null
                       :sentinel-write-fd sentinel-write
                       :tty-proxy-path proxy-path
                       :tty-proxy-demand-read-fd proxy-demand-read
                       :tty-proxy-write-fd proxy-write
                       :command (format nil "~{~a~^ | ~}"
                                        (mapcar #'fourth commands))))
                (setf complete t)
                plan))
          (unless complete
            (dolist (descriptor descriptors)
              (fd-close descriptor))))))))


;;; Lisp workers

(defun pipeline--gate-wait (gate)
  "Wait for GATE and return true unless setup cancelled it."
  (if (null gate)
      t
      (progn
        (ccl:wait-on-semaphore (pipeline-gate-semaphore gate))
        (ccl:with-lock-grabbed ((pipeline-gate-lock gate))
          (not (pipeline-gate-cancelled gate))))))

(defun pipeline--gate-release (gate count)
  "Open GATE once and wake COUNT waiting builtin workers."
  (let ((wake nil))
    (ccl:with-lock-grabbed ((pipeline-gate-lock gate))
      (unless (or (pipeline-gate-open gate)
                  (pipeline-gate-cancelled gate))
        (setf (pipeline-gate-open gate) t
              wake t)))
    (when wake
      (loop repeat count
            do (ccl:signal-semaphore
                (pipeline-gate-semaphore gate)))))
  (values))

(defun pipeline--gate-cancel (gate count)
  "Cancel GATE once and wake COUNT workers without running them."
  (let ((wake nil))
    (ccl:with-lock-grabbed ((pipeline-gate-lock gate))
      (unless (pipeline-gate-cancelled gate)
        (setf (pipeline-gate-cancelled gate) t
              wake (not (pipeline-gate-open gate)))))
    (when wake
      (loop repeat count
            do (ccl:signal-semaphore
                (pipeline-gate-semaphore gate)))))
  (values))

(defun pipeline--broken-pipe-p (condition)
  "True when CONDITION is CCL's stream report for EPIPE."
  (and (typep condition 'stream-error)
       (search "broken pipe"
               (princ-to-string condition)
               :test #'char-equal)))

(defun pipeline--close-owners (owners)
  "Close every (stream fd) OWNER and return its first close error."
  (let ((failure nil))
    (dolist (owner owners)
      (let ((stream (first owner))
            (descriptor (shiftf (second owner) nil)))
        (when descriptor
          (handler-case
              (close stream)
            (error (condition)
              (fd-close descriptor)
              (unless failure
                (setf failure condition)))))))
    failure))

(defun pipeline--task-done-p (task)
  "Return TASK's published completion flag."
  (pipeline-task-done task))

(defun pipeline--tasks-done-p (group)
  "True when every Lisp task in GROUP has published completion."
  (every #'pipeline--task-done-p
         (pipeline-task-group-tasks group)))

(defun pipeline--anchor-signal (group)
  "Return the fatal signal which ended GROUP's builtin anchor."
  (let ((process (pipeline-task-group-anchor-process group)))
    (when process
      (multiple-value-bind (state code)
          (shell-process-status process)
        (and (eq state ':signaled)
             (not (= code +process-sigpipe+))
             code)))))

(defun pipeline--tasks-lifecycle-done-p (group)
  "Publish anchor death to live tasks, then report their completion."
  (unless (pipeline--tasks-done-p group)
    (let ((signal (pipeline--anchor-signal group)))
      (when signal
        (pipeline--abort-tasks group :signal signal))))
  (pipeline--tasks-done-p group))

(defun pipeline--control-close (group)
  "Close GROUP's sentinel control writer exactly once."
  (let ((descriptor nil))
    (ccl:with-lock-grabbed ((pipeline-task-group-lock group))
      (setf descriptor
            (shiftf (pipeline-task-group-control-fd group) nil)))
    (fd-close descriptor))
  (values))

(defun pipeline--finish-proxy (group)
  "Terminate the tty reader after its builtin consumer is finished."
  (let ((process (pipeline-task-group-tty-proxy group)))
    (when process
      (ignore-errors (shell-process-kill process 15))))
  (values))

(defun pipeline--task-notify (task)
  "Publish TASK completion to its sentinel and job waiter."
  (let ((group (pipeline-task-group task)))
    (when (eq task (pipeline-task-group-first-task group))
      (pipeline--finish-proxy group))
    (when (pipeline--tasks-done-p group)
      (pipeline--control-close group))
    (job-notify (pipeline-task-group-job group)))
  (values))

(defun pipeline--task-abort-code (task)
  "Return the signal assigned to a live TASK by group cancellation."
  (let ((group (pipeline-task-group task)))
    (and (pipeline-task-group-aborted group)
         (pipeline-task-group-abort-signal group))))

(defun pipeline--run-task (task)
  "Run TASK, close all of its descriptors and publish one raw result."
  (let ((state ':running)
        (code nil)
        (value nil)
        (failure nil))
    (unwind-protect
        (when (pipeline--gate-wait (pipeline-task-gate task))
          (handler-case
              (multiple-value-setq (code value)
                (funcall (pipeline-task-function task)))
            (error (condition)
              (if (and (pipeline-task-expected-broken-pipe task)
                       (pipeline--broken-pipe-p condition))
                  (setf state ':signaled
                        code +process-sigpipe+)
                  (setf state ':exited
                        code 1
                        failure condition))))
          (when (eq state ':running)
            (setf state ':exited)))
      (let ((close-error
              (pipeline--close-owners (pipeline-task-owners task))))
        (when (and close-error (null failure))
          (if (and (pipeline-task-expected-broken-pipe task)
                   (pipeline--broken-pipe-p close-error))
              (setf state ':signaled
                    code +process-sigpipe+)
              (setf state ':exited
                    code 1
                    failure close-error))))
      (let ((abort-code (pipeline--task-abort-code task)))
        (when abort-code
          (setf state ':signaled
                code abort-code
                failure nil)))
      (when (eq state ':running)
        (setf state ':signaled
              code +process-sigkill+))
      (setf (pipeline-task-state task) state
            (pipeline-task-code task) code
            (pipeline-task-value task) value
            (pipeline-task-error task) failure
            (pipeline-task-done task) t)
      (pipeline--task-notify task)))
  (values))

(defun pipeline--stream-owner (descriptor direction)
  "Duplicate DESCRIPTOR and return a UTF-8 stream and owner record."
  (let ((duplicate (fd-duplicate descriptor)))
    (handler-case
        (let ((stream
                (ecase direction
                  (:input
                   (fd-input-stream duplicate :auto-close nil))
                  (:output
                   (fd-output-stream duplicate :auto-close nil)))))
          (values stream (list stream duplicate)))
      (error (condition)
        (fd-close duplicate)
        (error condition)))))

(defun pipeline--run-context-spawn
    (context path arguments event process-cell)
  "Spawn PATH into CONTEXT's running pipeline group.

The group lock makes a late spawn atomic with pipeline suspension and abort.
CCL interrupts stay disabled until PROCESS-CELL owns the child and its monitor
has started, so an asynchronous task abort cannot leave an unowned child."
  (let ((group (pipeline-run-context-group context)))
    (loop until (first process-cell)
          do (ccl:without-interrupts
               (ccl:with-lock-grabbed
                   ((pipeline-task-group-lock group))
                 (cond ((pipeline-task-group-aborted group)
                        (error "pipeline task was aborted"))
                       ((not (pipeline-task-group-suspended group))
                        (let ((process-group
                                (pipeline-task-group-process-group group)))
                          (unless (and (integerp process-group)
                                       (plusp process-group))
                            (error "pipeline process group is unavailable"))
                          (setf (first process-cell)
                                (shell-process-spawn
                                 path arguments
                                 :process-group process-group
                                 :fd0 (pipeline-run-context-input-fd context)
                                 :fd1 (pipeline-run-context-output-fd context)
                                 :fd2 (pipeline-run-context-error-fd context)
                                 :close-fds
                                 (pipeline-run-context-close-fds context)
                                 :event event))
                          (shell-process-start-monitor
                           (first process-cell) event))))))
             (unless (first process-cell)
               (ccl:process-allow-schedule)))
    (first process-cell)))

(defun pipeline--run-context-wait (context process event)
  "Wait for PROCESS and return its shell status inside CONTEXT's job."
  (let ((group (pipeline-run-context-group context))
        (propagated-stop-generation nil))
    (loop
      (multiple-value-bind (state code generation)
          (shell-process-status process)
        (case state
          (:exited
           (return (or code 0)))
          (:signaled
           (return (+ 128 (or code 0))))
          (:stopped
           ;; A child can stop itself without stopping the sentinel. Publish
           ;; each distinct stop to the outer group exactly once. GENERATION
           ;; distinguishes a stale stop snapshot after fg from a new stop
           ;; which followed a very short continued state.
           (unless (eql generation propagated-stop-generation)
             (setf propagated-stop-generation generation)
             (job--signal-group (pipeline-task-group-job group)
                                +process-sigtstp+
                                :missing-ok t))))
      (ccl:wait-on-semaphore event)))))

(defun pipeline--run-external (context path arguments)
  "Run external PATH with ARGUMENTS inside pipeline CONTEXT."
  (let ((process-cell (list nil))
        (event (ccl:make-semaphore)))
    (unwind-protect
        (progn
          ;; Preserve ordering between buffered Lisp output and a child which
          ;; writes directly to the same underlying descriptors.
          (force-output *standard-output*)
          (unless (eq *error-output* *standard-output*)
            (force-output *error-output*))
          (let ((demand (pipeline-run-context-input-demand context)))
            (when demand
              (pipeline--demand-input-start demand)))
          (pipeline--run-context-spawn
           context path arguments event process-cell)
          (pipeline--run-context-wait
           context (first process-cell) event))
      (when (first process-cell)
        (shell-process-kill-reap (first process-cell))))))

(defun pipeline--builtin-task (stage group &key standard-input
                                            standard-output
                                            error-output
                                            presentation-enabled
                                            package
                                            argv
                                            close-fds)
  "Create STAGE's gated builtin task and all of its UTF-8 streams."
  (let ((owners nil)
        (complete nil)
        (input-demand nil))
    (unwind-protect
        (multiple-value-bind (input input-owner)
            (if (= (pipeline-stage-input-fd stage) 0)
                (values standard-input nil)
                (pipeline--stream-owner
                 (pipeline-stage-input-fd stage) ':input))
          (when input-owner
            (push input-owner owners))
          (when (pipeline-stage-input-demand-fd stage)
            (multiple-value-bind (demand-output demand-owner)
                (pipeline--stream-owner
                 (pipeline-stage-input-demand-fd stage) ':output)
              (push demand-owner owners)
              (setf input
                    (make-instance 'pipeline-demand-input-stream
                                   :input input
                                   :demand-output demand-output)
                    input-demand input)))
          (multiple-value-bind (output output-owner)
              (if (= (pipeline-stage-output-fd stage) 1)
                  (values standard-output nil)
                  (pipeline--stream-owner
                   (pipeline-stage-output-fd stage) ':output))
            (when output-owner
              (push output-owner owners))
            (multiple-value-bind (error-stream error-owner)
                (cond ((= (pipeline-stage-error-fd stage)
                          (pipeline-stage-output-fd stage))
                       (values output nil))
                      ((= (pipeline-stage-error-fd stage) 2)
                       (values error-output nil))
                      (t
                       (pipeline--stream-owner
                        (pipeline-stage-error-fd stage) ':output)))
              (when error-owner
                (push error-owner owners))
              (let* ((input-fd
                       (if input-owner (second input-owner) 0))
                     (output-fd
                       (if output-owner (second output-owner) 1))
                     (error-fd
                       (cond ((= (pipeline-stage-error-fd stage)
                                 (pipeline-stage-output-fd stage))
                              output-fd)
                             (error-owner (second error-owner))
                             (t 2)))
                     (run-context
                       (make-pipeline-run-context
                        :group group
                        :input-fd input-fd
                        :output-fd output-fd
                        :error-fd error-fd
                        :close-fds close-fds
                        :input-demand input-demand))
                     (presentation-disabled
                       (or (/= (pipeline-stage-output-fd stage) 1)
                           (/= (pipeline-stage-error-fd stage) 2)))
                     (task
                       (make-pipeline-task
                        :name (format nil "cclsh builtin ~a"
                                      (pipeline-stage-label stage))
                        :gate (pipeline-task-group-gate group)
                        :owners (nreverse owners)
                        :expected-broken-pipe
                        (pipeline-stage-output-pipe-p stage)
                        :group group
                        :function
                        (lambda ()
                          (let ((*standard-input* input)
                                (*standard-output* output)
                                (*error-output* error-stream)
                                (*package* package)
                                (*argv* argv)
                                (*run-external-executor*
                                  (lambda (path arguments)
                                    (pipeline--run-external
                                     run-context path arguments)))
                                (*presentation-enabled*
                                  (and presentation-enabled
                                       (not presentation-disabled))))
                            (let ((status
                                    (command-execute-builtin
                                     (pipeline-stage-target stage)
                                     (pipeline-stage-arguments stage))))
                              (force-output output)
                              (unless (eq error-stream output)
                                (force-output error-stream))
                              (values status nil)))))))
                (setf complete t)
                task))))
      (unless complete
        (pipeline--close-owners owners)))))

(defun pipeline--capture-task (descriptor group)
  "Create the UTF-8 collector for a capture pipe DESCRIPTOR."
  (multiple-value-bind (input owner)
      (pipeline--stream-owner descriptor ':input)
    (make-pipeline-task
     :name "cclsh capture collector"
     :owners (list owner)
     :group group
     :function
     (lambda ()
       (let ((output (make-string-output-stream))
             (buffer (make-string 4096)))
         (loop for count = (read-sequence buffer input)
               while (plusp count)
               do (write-sequence buffer output :end count))
         (values 0 (get-output-stream-string output)))))))

(defun pipeline--start-tasks (group)
  "Start all GROUP workers. Builtins remain behind their gate."
  (handler-case
      (dolist (task (pipeline-task-group-tasks group))
        (setf (pipeline-task-thread task)
              (funcall *pipeline-task-starter*
                       (pipeline-task-name task)
                       #'pipeline--run-task task)))
    (error (condition)
      (dolist (task (pipeline-task-group-tasks group))
        (unless (or (pipeline-task-thread task)
                    (pipeline-task-done task))
          (pipeline--close-owners (pipeline-task-owners task))
          (setf (pipeline-task-state task) ':signaled
                (pipeline-task-code task) +process-sigkill+
                (pipeline-task-done task) t
                (pipeline-task-joined task) t)
          (pipeline--task-notify task)))
      (error condition)))
  (values))

(defun pipeline--task-result (task)
  "Return TASK's raw result for JOB's persistent status provider."
  (values (pipeline-task-state task)
          (pipeline-task-code task)))

(defun pipeline--task-errors (group)
  "Return the first task error in pipeline order."
  (loop for task in (pipeline-task-group-tasks group)
        when (pipeline-task-error task)
          return (pipeline-task-error task)))

(defun pipeline--join-tasks (group &key signal-errors)
  "Join GROUP's workers and optionally re-signal their first error."
  (dolist (task (pipeline-task-group-tasks group))
    (let ((thread (pipeline-task-thread task)))
      (cond ((and thread (not (pipeline-task-joined task)))
             (ccl:join-process thread)
             (setf (pipeline-task-joined task) t))
            ((and (null thread) (not (pipeline-task-done task)))
             (pipeline--close-owners (pipeline-task-owners task))
             (setf (pipeline-task-state task) ':signaled
                   (pipeline-task-code task) +process-sigkill+
                   (pipeline-task-done task) t
                   (pipeline-task-joined task) t)
             (pipeline--task-notify task)))))
  (pipeline--control-close group)
  (when signal-errors
    (let ((condition (pipeline--task-errors group)))
      (when condition
        (error condition))))
  (values))

(defun pipeline--suspend-tasks (group)
  "Suspend every live worker once when GROUP receives a terminal stop."
  (let ((suspend nil))
    (ccl:with-lock-grabbed ((pipeline-task-group-lock group))
      (unless (or (pipeline-task-group-suspended group)
                  (pipeline-task-group-aborted group))
        (setf (pipeline-task-group-suspended group) t
              suspend t)
        ;; Repeat the group stop while holding the late-spawn lock. A child
        ;; created after the terminal's original Ctrl-Z cannot miss the stop.
        (let ((process-group
                (pipeline-task-group-process-group group)))
          (when (and (integerp process-group) (plusp process-group))
            (process-group-kill process-group +process-sigtstp+)))))
    (when suspend
      (dolist (task (pipeline-task-group-tasks group))
        (when (and (not (pipeline--task-done-p task))
                   (pipeline-task-thread task))
          (ignore-errors
            (ccl:process-suspend (pipeline-task-thread task)))))))
  (values))

(defun pipeline--resume-tasks (group)
  "Resume every worker suspended with GROUP."
  (let ((resume nil))
    (ccl:with-lock-grabbed ((pipeline-task-group-lock group))
      (when (and (pipeline-task-group-suspended group)
                 (not (pipeline-task-group-aborted group)))
        (setf (pipeline-task-group-suspended group) nil
              resume t)))
    (when resume
      (dolist (task (pipeline-task-group-tasks group))
        (when (and (not (pipeline--task-done-p task))
                   (pipeline-task-thread task))
          (ignore-errors
            (ccl:process-resume (pipeline-task-thread task)))))))
  (values))

(defun pipeline--interrupt-code (job)
  "Return SIGINT or SIGQUIT when one of JOB's children reported it."
  (loop for process in (job-processes job)
        do (multiple-value-bind (state code)
               (shell-process-status process)
             (when (and (eq state ':signaled)
                        (member code
                                (list +process-sigint+
                                      +process-sigquit+)))
               (return code)))))

(defun pipeline--abort-tasks (group &key signal)
  "Asynchronously unwind every live task, closing its streams."
  (let ((abort nil)
        (resume nil))
    (ccl:with-lock-grabbed ((pipeline-task-group-lock group))
      (unless (pipeline-task-group-aborted group)
        (setf (pipeline-task-group-aborted group) t
              (pipeline-task-group-abort-signal group)
              (or signal +process-sigkill+)
              resume (pipeline-task-group-suspended group)
              (pipeline-task-group-suspended group) nil
              abort t)
        ;; Serialize a second signal with late child creation. Either the
        ;; child joins before this signal or sees ABORTED and never spawns.
        (let ((process-group
                (pipeline-task-group-process-group group)))
          (when (and (integerp process-group) (plusp process-group))
            (process-group-kill
             process-group
             (pipeline-task-group-abort-signal group))))))
    (when abort
      (dolist (task (pipeline-task-group-tasks group))
        (when (and (not (pipeline--task-done-p task))
                   (pipeline-task-thread task))
          (when resume
            (ignore-errors
              (ccl:process-resume (pipeline-task-thread task))))
          (ignore-errors
            (ccl:process-kill (pipeline-task-thread task)))))
      (pipeline--control-close group)))
  (values))


;;; Spawning and execution

(defun pipeline--remember-process (process group)
  "Add PROCESS to GROUP's unmonitored child list."
  (push process (pipeline-task-group-processes group))
  process)

(defun pipeline--spawn-one (path arguments group &key fd0 fd1 fd2 close-fds)
  "Spawn one child into GROUP and remember it for cleanup."
  (let* ((process-group
           (or (pipeline-task-group-process-group group) 0))
         (process
           (shell-process-spawn
            path arguments
            :process-group process-group
            :fd0 fd0
            :fd1 fd1
            :fd2 fd2
            :close-fds close-fds
            :event (job-event (pipeline-task-group-job group)))))
    (unless (pipeline-task-group-process-group group)
      (setf (pipeline-task-group-process-group group)
            (shell-process-pid process)))
    (pipeline--remember-process process group)))

(defun pipeline--spawn-plan (plan group)
  "Spawn PLAN's sentinel, tty proxy and real external stages."
  (when (pipeline-plan-sentinel-path plan)
    (setf (pipeline-task-group-anchor-process group)
          (pipeline--spawn-one
           (pipeline-plan-sentinel-path plan) nil group
           :fd0 (pipeline-plan-sentinel-read-fd plan)
           :fd1 (pipeline-plan-sentinel-null-fd plan)
           :fd2 2
           :close-fds (pipeline-plan-descriptors plan)))
    (when (pipeline-plan-tty-proxy-write-fd plan)
      (setf (pipeline-task-group-tty-proxy group)
            (pipeline--spawn-one
             (pipeline-plan-tty-proxy-path plan)
             (list "-c"
                   "IFS= read -r line || exit; exec \"$1\" </dev/tty"
                   "cclsh-proxy"
                   (pipeline-plan-sentinel-path plan))
             group
             :fd0 (pipeline-plan-tty-proxy-demand-read-fd plan)
             :fd1 (pipeline-plan-tty-proxy-write-fd plan)
             :fd2 2
             :close-fds (pipeline-plan-descriptors plan)))))
  (dolist (stage (pipeline-plan-stages plan))
    (when (eq (pipeline-stage-kind stage) ':external)
      (pipeline--spawn-one
       (pipeline-stage-target stage)
       (pipeline-stage-arguments stage)
       group
       :fd0 (pipeline-stage-input-fd stage)
       :fd1 (pipeline-stage-output-fd stage)
       :fd2 (pipeline-stage-error-fd stage)
       :close-fds (pipeline-plan-descriptors plan))))
  (setf (pipeline-task-group-processes group)
        (nreverse (pipeline-task-group-processes group)))
  (values))

(defun pipeline--kill-children (group)
  "Kill GROUP once and synchronously reap each unmonitored child."
  (let* ((processes (pipeline-task-group-processes group))
         (leader
           (find (pipeline-task-group-process-group group)
                 processes :key #'shell-process-pid)))
    (when processes
      (ignore-errors
        (shell-process-kill (or leader (first processes))
                            +process-sigkill+
                            :group t))
      (dolist (process processes)
        (ignore-errors (shell-process-kill-reap process)))))
  (values))

(defun pipeline--build-tasks (plan group)
  "Create all builtin and capture tasks without starting side effects."
  (let ((first-task nil)
        (capture-task nil)
        (standard-input *standard-input*)
        (standard-output *standard-output*)
        (error-output *error-output*)
        (package *package*)
        (argv *argv*)
        (presentation-enabled *presentation-enabled*))
    (setf (pipeline-task-group-tasks group) nil)
    (dolist (stage (pipeline-plan-stages plan))
      (when (eq (pipeline-stage-kind stage) ':builtin)
        (let ((task
                (pipeline--builtin-task
                 stage group
                 :standard-input standard-input
                 :standard-output standard-output
                 :error-output error-output
                 :presentation-enabled presentation-enabled
                 :package package
                 :argv argv
                 :close-fds (pipeline-plan-descriptors plan))))
          (setf (pipeline-stage-task stage) task)
          (unless first-task
            (setf first-task task))
          (push task (pipeline-task-group-tasks group)))))
    (when (pipeline-plan-capture-read-fd plan)
      (setf capture-task
            (pipeline--capture-task
             (pipeline-plan-capture-read-fd plan) group))
      (push capture-task (pipeline-task-group-tasks group)))
    (setf (pipeline-task-group-tasks group)
          (nreverse (pipeline-task-group-tasks group)))
    (when (pipeline-plan-tty-proxy-write-fd plan)
      (setf (pipeline-task-group-first-task group) first-task))
    capture-task))

(defun pipeline--attach-tasks (plan group)
  "Attach GROUP's lifecycle hooks and logical result to its job."
  (let* ((job (pipeline-task-group-job group))
         (tasks (pipeline-task-group-tasks group))
         (final (first (last (pipeline-plan-stages plan)))))
    (when tasks
      (job-add-auxiliary
       job
       (lambda () (pipeline--tasks-lifecycle-done-p group))
       :stop (lambda () (pipeline--suspend-tasks group))
       :resume (lambda () (pipeline--resume-tasks group))
       :abort
       (lambda ()
         (pipeline--abort-tasks
          group :signal (or (pipeline--interrupt-code job)
                            +process-sigint+))))
      (job-add-cleanup
       job (lambda ()
             (pipeline--join-tasks group :signal-errors t))))
    (when (eq (pipeline-stage-kind final) ':builtin)
      (let ((task (pipeline-stage-task final)))
        (setf (job-result-provider job)
              (lambda () (pipeline--task-result task))))))
  (values))

(defun pipeline--retain-control-writer (plan group)
  "Duplicate PLAN's sentinel writer for the task completion latch."
  (when (pipeline-plan-sentinel-write-fd plan)
    (setf (pipeline-task-group-control-fd group)
          (fd-duplicate (pipeline-plan-sentinel-write-fd plan))))
  (values))

(defun pipeline--builtin-count (plan)
  "Return the number of gated builtin workers in PLAN."
  (count ':builtin (pipeline-plan-stages plan)
         :key #'pipeline-stage-kind))

(defun pipeline--captured-string (task)
  "Return TASK's captured UTF-8 text with trailing newlines removed."
  (and task
       (string-right-trim '(#\newline)
                          (or (pipeline-task-value task) ""))))

(defun pipeline--execute (resolved &key capture)
  "Execute a validated pipeline and return its status and capture."
  (let* ((plan (pipeline--prepare resolved :capture capture))
         (job (job-make :command (pipeline-plan-command plan)))
         (gate (pipeline--make-gate))
         (group (make-pipeline-task-group :job job :gate gate))
         (capture-task nil)
         (completed nil)
         (retained nil)
         (status nil))
    (unwind-protect
        (progn
          (pipeline--retain-control-writer plan group)
          (setf capture-task (pipeline--build-tasks plan group))
          (pipeline--attach-tasks plan group)
          (pipeline--start-tasks group)
          (pipeline--spawn-plan plan group)
          (setf (job-processes job)
                (pipeline-task-group-processes group)
                (job-process-group job)
                (pipeline-task-group-process-group group))
          (pipeline--close-plan plan)
          (setf status
                (job-run-foreground
                 job
                 :on-attend
                 (lambda ()
                   (pipeline--gate-release
                    gate (pipeline--builtin-count plan)))
                 :on-stop (if capture ':continue ':suspend)))
          (setf retained (eq (job-status job) ':stopped)
                completed (not retained))
          (command-status-record status)
          (values status (pipeline--captured-string capture-task)))
      (pipeline--close-plan plan)
      (unless (or completed retained (eq (job-status job) ':done))
        (pipeline--gate-cancel gate (pipeline--builtin-count plan))
        (pipeline--kill-children group)
        (pipeline--abort-tasks group :signal +process-sigkill+)
        (ignore-errors
          (pipeline--join-tasks group :signal-errors nil))))))

(defun pipeline-run (stages)
  "Run STAGES as a pipeline and return the last command's exit status.
   Every external stage shares one foreground process group, so
   terminal signals apply to the pipeline as one job."
  (values (pipeline--execute (pipeline--resolve-stages stages))))

(defun pipeline-capture (stages)
  "Run STAGES as a pipeline capturing standard output. Returns
   (values string status)."
  (multiple-value-bind (status captured)
      (pipeline--execute (pipeline--resolve-stages stages) :capture t)
    (values (or captured "") status)))

(defun stage-sequence-run (stages mode)
  "Run STAGES one after another in the foreground. MODE is :ALWAYS,
   :WHILE-SUCCESSFUL (stop on failure) or :UNTIL-SUCCESSFUL (stop on
   success). Returns the deciding exit status."
  (let ((status 0))
    (loop for (name . arguments) in stages
          do (multiple-value-bind (kind target)
                 (command-resolve-fresh name)
               (let ((*job-command-label*
                       (format nil "~{~a~^ ~}" (cons name arguments))))
                 (setf status
                       (ecase kind
                         (:builtin  (command-execute-builtin target
                                                             arguments))
                         (:external (command-execute-external target
                                                              arguments))
                         (:unknown  (error 'command-not-found-error
                                           :name name)))))
               (command-status-record status)
               (case mode
                 (:while-successful
                  (unless (zerop status)
                    (return)))
                 (:until-successful
                  (when (zerop status)
                    (return))))))
    status))
