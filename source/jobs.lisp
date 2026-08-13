;;;; -- External execution and jobs --
;;;
;;; Running external programs and controlling them as jobs. A job is
;;; one foreground or background command or pipeline. Every process
;;; in a job belongs to one process group. Blocking waitpid monitors
;;; publish stopped, continued and completed transitions through a
;;; shared event, so job control neither polls nor guesses process
;;; state from Linux-specific interfaces.

(in-package #:cclsh)

(defconstant +job-sighup+ 1
  "Linux SIGHUP signal number used when the shell exits.")

;;; The job table

(define-condition job-control-error (error)
  ((operation
    :initarg :operation
    :reader job-control-error-operation)
   (process-group
    :initarg :process-group
    :reader job-control-error-process-group)
   (signal
    :initarg :signal
    :reader job-control-error-signal)
   (code
    :initarg :code
    :reader job-control-error-code))
  (:documentation "Signaled when a job process-group operation fails.")
  (:report
   (lambda (condition stream)
     (format stream "cannot ~a process group ~d with signal ~d: ~a"
             (job-control-error-operation condition)
             (job-control-error-process-group condition)
             (job-control-error-signal condition)
             (process--error-string (job-control-error-code condition))))))

(defstruct (job-auxiliary
             (:constructor job--make-auxiliary
                 (done-p &key stop resume abort)))
  "One in-process task aggregate controlled alongside a job."
  done-p
  stop
  resume
  abort)

(defstruct (job
             (:constructor job-make
                 (&key processes process-group command (cleanups nil)
                       (auxiliaries nil)
                       (result-provider nil)
                       (event (ccl:make-semaphore)))))
  "One shell job. ID is the table number, NIL until the job is
   registered. PROCESSES are SHELL-PROCESS objects in pipeline order
   and PROCESS-GROUP is their shared process group id. EVENT wakes
   waiters after a process transition; LOCK protects aggregate job
   state. COMMAND is the display text. STATUS is :RUNNING, :STOPPED or
   :DONE, and REPORTED is the status last shown to the user. TOUCHED
   orders the current and previous marks. ATTRIBUTES is the terminal
   mode retained when the job stops. AUXILIARIES are predicates for
   in-process pipeline tasks. RESULT-PROVIDER returns the raw state
   and code of a logical final stage. CLEANUPS run once at completion."
  (id               nil)
  (processes        nil)
  (process-group    nil)
  (command          "")
  (status           ':running)
  (reported         ':running)
  (touched          0)
  (attributes       nil)
  (cleanups         cleanups)
  (auxiliaries      auxiliaries)
  (result-provider  result-provider)
  (event            event)
  (lock             (ccl:make-lock "cclsh job"))
  (monitor-start-lock
                    (ccl:make-lock "cclsh job monitor startup"))
  (monitors-started nil)
  (monitor-error    nil)
  (auxiliaries-interrupted nil))

(defvar *jobs* nil
  "Registered jobs, most recently registered first.")

(defvar *jobs-touch-counter* 0
  "Monotonic recency source for the current and previous job marks.")

(defvar *job-command-label* nil
  "Display text for the job a command execution is about to start,
   bound around dispatch so stopped jobs list the line as typed.")

(defvar *run-external-executor* nil
  "Dynamically bound external executor for RUN, or NIL for a new job.

Pipeline builtin workers bind this to an executor which joins the surrounding
pipeline job and inherits that stage's prepared standard descriptors.")

(defun job-touch (job)
  "Stamp JOB as the most recently handled job."
  (setf (job-touched job) (incf *jobs-touch-counter*))
  (values))

(defun job-register (job)
  "Enter JOB into the job table under the next free job id."
  (setf (job-id job)
        (1+ (reduce #'max *jobs* :key #'job-id :initial-value 0)))
  (job-touch job)
  (push job *jobs*)
  job)

(defun job-unregister (job)
  "Remove JOB from the job table."
  (setf *jobs* (delete job *jobs*))
  (values))

(defun jobs--ordered ()
  "Jobs ordered as the current and previous marks see them: stopped
   jobs before the rest, most recently touched first."
  (sort (copy-list *jobs*)
        (lambda (first-job second-job)
          (let ((first-stopped  (eq (job-status first-job) ':stopped))
                (second-stopped (eq (job-status second-job) ':stopped)))
            (if (eq first-stopped second-stopped)
                (> (job-touched first-job) (job-touched second-job))
                first-stopped)))))

(defun job-current ()
  "The current job, the one % and fg and bg without arguments mean."
  (first (jobs--ordered)))

(defun job-mark (job)
  "The + mark of the current job, the - mark of the previous one, or
   a space."
  (let ((ordered (jobs--ordered)))
    (cond ((eq job (first ordered))
           "+")
          ((eq job (second ordered))
           "-")
          (t
           " "))))


;;; Job status

(defun job--process-states (job)
  "Return an atomic state snapshot for every process in JOB."
  (mapcar #'shell-process-live-state (job-processes job)))

(defun job--auxiliaries-done-p (job)
  "True when every in-process task attached to JOB has completed."
  (every (lambda (auxiliary)
           (funcall (job-auxiliary-done-p auxiliary)))
         (job-auxiliaries job)))

(defun job--aggregate-state (states auxiliaries-done-p)
  "Aggregate process STATES and AUXILIARIES-DONE-P into a job state.
   Completed stages do not keep the remaining live stages from being
   stopped. A stopped external group wins over live Lisp tasks, while
   all external processes finishing does not finish the job until its
   Lisp tasks have also completed."
  (cond ((find ':running states)
         ':running)
        ((find ':stopped states)
         ':stopped)
        ((not auxiliaries-done-p)
         ':running)
        (t
         ':done)))

(defun job-add-auxiliary (job done-p &key stop resume abort)
  "Keep JOB live until the function DONE-P returns true.
   Attach auxiliaries before monitor startup. The auxiliary must call
   JOB-NOTIFY after its state changes. STOP, RESUME and ABORT control
   an in-process worker when the external process group changes state."
  (check-type done-p function)
  (ccl:with-lock-grabbed ((job-lock job))
    (when (or (job-monitors-started job)
              (eq (job-status job) ':done))
      (error "cannot attach an auxiliary after job startup"))
    (push (job--make-auxiliary done-p
                               :stop stop
                               :resume resume
                               :abort abort)
          (job-auxiliaries job)))
  job)

(defun job-notify (job)
  "Wake a waiter after an attached in-process task changes state."
  (ccl:signal-semaphore (job-event job))
  (values))

(defun job--control-auxiliaries (job operation)
  "Apply OPERATION to every attached in-process task completely."
  (let ((failure nil))
    (dolist (auxiliary (job-auxiliaries job))
      (let ((function
              (ecase operation
                (:stop   (job-auxiliary-stop auxiliary))
                (:resume (job-auxiliary-resume auxiliary))
                (:abort  (job-auxiliary-abort auxiliary)))))
        (when function
          (handler-case
              (funcall function)
            (error (condition)
              (unless failure
                (setf failure condition)))))))
    (when failure
      (error failure)))
  (values))

(defun job--interrupted-p (job)
  "True when a child reports the terminal interrupt or quit signal."
  (find-if
   (lambda (process)
     (multiple-value-bind (state code)
         (shell-process-status process)
       (and (eq state ':signaled)
            (member code (list +process-sigint+ +process-sigquit+)))))
   (job-processes job)))

(defun job--run-cleanups (cleanups)
  "Run CLEANUPS completely, then re-signal the first failure."
  (let ((failure nil))
    (dolist (cleanup cleanups)
      (handler-case
          (funcall cleanup)
        (error (condition)
          (unless failure
            (setf failure condition)))))
    (when failure
      (error failure))))

(defun job--finish (job)
  "Run the cleanup closures attached to JOB exactly once."
  (let ((cleanups
          (ccl:with-lock-grabbed ((job-lock job))
            (shiftf (job-cleanups job) nil))))
    (job--run-cleanups cleanups))
  (values))

(defun job-add-cleanup (job cleanup)
  "Arrange for CLEANUP to run once when JOB completes."
  (let ((run-now nil))
    (ccl:with-lock-grabbed ((job-lock job))
      (if (eq (job-status job) ':done)
          (setf run-now t)
          (push cleanup (job-cleanups job))))
    (when run-now
      (funcall cleanup)))
  job)

(defun job-refresh (job)
  "Refresh and return JOB's aggregate process state. A running job
   touched by a group stop becomes the most recent stopped job."
  (let ((cleanups nil)
        (stop-auxiliaries nil)
        (abort-auxiliaries nil)
        (new-status
          (job--aggregate-state (job--process-states job)
                                (job--auxiliaries-done-p job))))
    (ccl:with-lock-grabbed ((job-lock job))
      (let ((old-status (job-status job)))
        (unless (eq old-status ':done)
          (setf (job-status job) new-status)
          (when (and (eq old-status ':running)
                     (eq new-status ':stopped))
            (job-touch job)
            (setf stop-auxiliaries t))
          (when (and (not (job-auxiliaries-interrupted job))
                     (job--interrupted-p job))
            (setf (job-auxiliaries-interrupted job) t)
            (setf abort-auxiliaries t))
          (when (eq new-status ':done)
            (setf cleanups (shiftf (job-cleanups job) nil))))))
    (when stop-auxiliaries
      (job--control-auxiliaries job ':stop))
    (when abort-auxiliaries
      (job--control-auxiliaries job ':abort))
    (job--run-cleanups cleanups)
    (job-status job)))

(defun job--result (job)
  "Return the raw state and code of JOB's logical final stage."
  (if (job-result-provider job)
      (funcall (job-result-provider job))
      (shell-process-status (first (last (job-processes job))))))

(defun job-exit-status (job)
  "Shell exit status of JOB's logical final stage."
  (multiple-value-bind (state code)
      (job--result job)
    (case state
      (:exited   (or code 0))
      (:signaled (+ 128 (or code 0)))
      (t         1))))


;;; Display

(defparameter *job-signal-names*
  '((1  . "Hangup")
    (2  . "Interrupt")
    (3  . "Quit")
    (6  . "Aborted")
    (9  . "Killed")
    (11 . "Segmentation fault")
    (13 . "Broken pipe")
    (15 . "Terminated"))
  "Report strings for the signals that commonly end jobs.")

(defun job--status-text (job)
  "The status column of JOB in the job table."
  (ecase (job-status job)
    (:running
     "Running")
    (:stopped
     "Stopped")
    (:done
     (multiple-value-bind (status code)
         (job--result job)
       (cond ((eq status ':signaled)
              (or (rest (assoc code *job-signal-names*))
                  (format nil "Signal ~d" code)))
             ((and (eq status ':exited) (plusp (or code 0)))
              (format nil "Exit ~d" code))
             (t
              "Done"))))))

(defun job-print (job stream &key show-pid)
  "Print the job table line of JOB to STREAM: id, mark, status and
   command, preceded by its stored process group id under SHOW-PID.
   Running jobs display with the trailing & they run under."
  (format stream "[~d]~a  ~@[~d ~]~24a~a~@[ &~]~%"
          (job-id job)
          (job-mark job)
          (and show-pid (job-process-group job))
          (job--status-text job)
          (job-command job)
          (eq (job-status job) ':running))
  (values))

(defun jobs-notify ()
  "Report jobs whose status changed since the last report and drop
   finished jobs from the table. Runs before each interactive prompt."
  (dolist (job (sort (copy-list *jobs*) #'< :key #'job-id))
    (let ((status (job-refresh job)))
      (unless (eq status (job-reported job))
        (setf (job-reported job) status)
        (job-print job *error-output*))
      (when (eq status ':done)
        (job-unregister job))))
  (force-output *error-output*)
  (values))


;;; Foreground execution

(defun job--signal-group (job signal &key missing-ok)
  "Send SIGNAL once to JOB's stored process group.
   MISSING-OK treats an already vanished group as success."
  (multiple-value-bind (success code)
      (process-group-kill (job-process-group job) signal)
    (unless (or success
                (and missing-ok (= code +process-esrch+)))
      (error 'job-control-error
             :operation "signal"
             :process-group (job-process-group job)
             :signal signal
             :code code)))
  (values))

(defun job--wait-for-change (job)
  "Block until a waitpid monitor publishes a process transition."
  (ccl:wait-on-semaphore (job-event job))
  (values))

(defun job--kill-reap (job)
  "Abort JOB, reap every member and run all cleanup closures."
  (let ((failure nil))
    (flet ((attempt (function)
             (handler-case
                 (funcall function)
               (error (condition)
                 (unless failure
                   (setf failure condition))))))
      (attempt (lambda ()
                 (job--signal-group job +process-sigkill+
                                    :missing-ok t)))
      (attempt (lambda ()
                 (job--control-auxiliaries job ':abort)))
      (dolist (process (job-processes job))
        (attempt (lambda ()
                   (shell-process-kill-reap process))))
      (let ((cleanups
              (ccl:with-lock-grabbed ((job-lock job))
                (setf (job-status job) ':done)
                (shiftf (job-cleanups job) nil))))
        (when (job-id job)
          (job-unregister job))
        (attempt (lambda ()
                   (job--run-cleanups cleanups))))
      (when failure
        (error failure))))
  (values))

(defun job--abort-monitor-start (job)
  "Kill and reap JOB after monitor startup failed partway through."
  (job--kill-reap job)
  (values))

(defun job-start-monitors (job)
  "Start one waitpid monitor per process in JOB, all sharing its event.
   This is idempotent. Processes are spawned first so every pipeline
   member has joined the process group before any child is reaped."
  ;; A mutex is deliberately separate from JOB-EVENT. The event is a
  ;; counting semaphore for child transitions, not a broadcast primitive;
  ;; using it for concurrent startup callers can strand all but one waiter.
  (ccl:with-lock-grabbed ((job-monitor-start-lock job))
    (let ((state
            (ccl:with-lock-grabbed ((job-lock job))
              (job-monitors-started job))))
      (cond ((eq state t)
             job)
            ((eq state ':failed)
             (error (job-monitor-error job)))
            (t
             (ccl:with-lock-grabbed ((job-lock job))
               (setf (job-monitors-started job) ':starting))
             (unless (job-process-group job)
               (setf (job-process-group job)
                     (shell-process-pid (first (job-processes job)))))
             (handler-case
                 (progn
                   (dolist (process (job-processes job))
                     (shell-process-start-monitor process (job-event job)))
                   (ccl:with-lock-grabbed ((job-lock job))
                     (setf (job-monitors-started job) t)
                     (setf (job-monitor-error job) nil))
                   job)
               (error (condition)
                 (let ((failure condition))
                   (handler-case
                       (job--abort-monitor-start job)
                     (error (abort-condition)
                       (setf failure abort-condition)))
                   (ccl:with-lock-grabbed ((job-lock job))
                     (setf (job-monitors-started job) ':failed)
                     (setf (job-monitor-error job) failure))
                   (error failure)))))))))

(defun job--continue-processes (job)
  "Continue JOB with one process-group signal and wait for WCONTINUED."
  (job--signal-group job +sigcont+)
  (job--control-auxiliaries job ':resume)
  (loop while (eq (job-refresh job) ':stopped)
        do (job--wait-for-change job))
  (values))

(defun job--suspend (job)
  "Book a stopped foreground JOB into the table and announce it.
   JOB-RUN-FOREGROUND has already retained the job's terminal mode and
   restored the shell mode captured immediately before attendance."
  (unless (job-id job)
    (job-register job))
  (unless (eq (job-status job) ':stopped)
    (setf (job-status job) ':stopped)
    (job-touch job))
  (setf (job-reported job) ':stopped)
  (terminal-fresh-line)
  (job-print job *error-output*)
  (force-output *error-output*)
  (+ 128 +sigtstp+))

(defun job-wait-attended (job &key (on-stop ':suspend))
  "Wait for JOB, which already owns the terminal, until it completes.
   Interactive sessions handle a stop per ON-STOP: :SUSPEND makes the
   wait return so the caller can book the job into the table, and
   :CONTINUE resumes the job at once with a one-time notice, for
   pipelines whose output the shell itself is collecting.
   Non-interactive sessions continue through stops. Returns
   (values status stopped) where STATUS is the exit status of the
   job's last process, or the stop status 148."
  (let ((interactive (terminal-tty-p))
        (noticed     nil))
    (job-start-monitors job)
    (loop
      (case (job-refresh job)
        (:done
         (return (values (job-exit-status job) nil)))
        (:stopped
         (cond ((and interactive (not (eq on-stop ':continue)))
                (return (values (+ 128 +sigtstp+) t)))
               (t
                (when (and interactive (not noticed))
                  (setf noticed t)
                  (format *error-output*
                          "cclsh: cannot suspend a capture; continuing~%")
                  (force-output *error-output*))
                (job--continue-processes job))))
        (:running
         (job--wait-for-change job))))))

(defun job--hand-terminal (job)
  "Give JOB's existing process group the terminal. A job can finish
   between the last state snapshot and this handoff; that is benign."
  (multiple-value-bind (success code)
      (terminal-foreground (job-process-group job))
    (cond (success
           t)
          ((and (member code (list +terminal-esrch+ +terminal-eperm+))
                (job-monitors-started job)
                (eq (job-refresh job) ':done))
           (let ((shell-group (terminal-own-process-group)))
             (multiple-value-bind (foreground foreground-code)
                 (terminal-current-foreground)
               (unless foreground
                 (error 'terminal-control-error
                        :operation "inspect the terminal for"
                        :process-group shell-group
                        :code foreground-code))
               (unless (= foreground shell-group)
                 (terminal-foreground-checked
                  shell-group :operation "return the terminal to"))))
           nil)
          (t
           (error 'terminal-control-error
                  :operation "give the terminal to"
                  :process-group (job-process-group job)
                  :code code)))))

(defun job-run-foreground (job &key continue on-attend
                                    (on-stop ':suspend))
  "Run JOB as the terminal's foreground job until it finishes or, on
   an interactive session, stops. With CONTINUE the job resumes from
   a stop: its retained terminal attributes are reapplied and its one
   process group receives SIGCONT. Returns the exit status, or 148
   when the job stopped and returned to the table. ON-ATTEND, when
   supplied, runs once after monitor startup and any foreground
   handoff. ON-STOP controls stopped jobs like JOB-WAIT-ATTENDED."
  (let ((interactive (terminal-tty-p))
        (shell-group (terminal-own-process-group))
        (shell-attributes nil)
        (attended    nil)
        (returned    nil)
        (status      nil)
        (stopped     nil))
    (unwind-protect
        (progn
          (when (and interactive
                     (or (not (job-monitors-started job))
                         (not (eq (job-refresh job) ':done))))
            ;; This must be adjacent to the handoff: it is the current
            ;; shell mode, not the mode cclsh happened to start under.
            (setf shell-attributes (terminal-attributes-checked))
            (setf attended (job--hand-terminal job)))
          ;; Keep an unmonitored group leader waitable until after the
          ;; foreground handoff; otherwise a very short command could
          ;; be reaped before TCSETPGRP names its group.
          (job-start-monitors job)
          (when attended
            (when continue
              (terminal-attributes-apply (job-attributes job)))
            (if continue
                (job--continue-processes job)
                (job--signal-group job +sigcont+ :missing-ok t)))
          (when on-attend
            (funcall on-attend))
          (multiple-value-setq (status stopped)
            (job-wait-attended job :on-stop on-stop))
          (setf returned t))
      (let ((cleanup-failure nil)
            (reclaim-failed nil))
        (flet ((remember-failure (condition)
                 (unless cleanup-failure
                   (setf cleanup-failure condition))))
          (when interactive
            (when (and attended stopped)
              (handler-case
                  (setf (job-attributes job)
                        (terminal-attributes-checked))
                (error (condition)
                  (remember-failure condition))))
            (when attended
              (handler-case
                  (terminal-foreground-checked
                   shell-group :operation "return the terminal to")
                (error (condition)
                  (setf reclaim-failed t)
                  (remember-failure condition))))
            ;; A completed foreground command is allowed to change the
            ;; terminal deliberately. Stops and exceptional exits return
            ;; to the exact mode in which this attendance began.
            (when (or stopped (not returned))
              (handler-case
                  (terminal-attributes-apply shell-attributes)
                (error (condition)
                  (remember-failure condition)))))
          (when (or (not returned) cleanup-failure)
            (handler-case
                (job--kill-reap job)
              (error (condition)
                (remember-failure condition))))
          ;; A dead foreground group can make the first reclaim fail.
          ;; Try again after aborting it, but preserve the first error.
          (when (and interactive attended reclaim-failed)
            (handler-case
                (terminal-foreground-checked
                 shell-group :operation "return the terminal to")
              (error (condition)
                (remember-failure condition))))
          (when cleanup-failure
            (error cleanup-failure)))))
    (cond (stopped
           (job--suspend job))
          (t
           (when (job-id job)
             (job-unregister job))
           status))))


;;; Spawning

(defun job--label (path arguments)
  "Display text for a job: the command line as typed when dispatch
   provided one, the program path and arguments otherwise."
  (or *job-command-label*
      (format nil "~{~a~^ ~}" (cons path arguments))))

(defun command-execute-external (path arguments)
  "Run the program at PATH with ARGUMENTS sharing the terminal as the
   foreground job. Returns the exit status, 148 after a Ctrl-Z stop."
  (let* ((job     (job-make :command (job--label path arguments)))
         (process (shell-process-spawn path arguments
                                       :process-group 0
                                       :event (job-event job))))
    (setf (job-processes job) (list process))
    (setf (job-process-group job) (shell-process-pid process))
    (job-run-foreground job)))

(defun command-execute-background (path arguments)
  "Start the program at PATH with ARGUMENTS as a background job and
   announce its job id and process group. Returns zero, the status of
   a successful launch."
  (let* ((job     (job-make :command (job--label path arguments)))
         (process (shell-process-spawn path arguments
                                       :process-group 0
                                       :event (job-event job))))
    (setf (job-processes job) (list process))
    (setf (job-process-group job) (shell-process-pid process))
    (job-start-monitors job)
    (job-register job)
    (when (terminal-tty-p)
      (format *error-output* "[~d] ~d~%"
              (job-id job) (job-process-group job))
      (force-output *error-output*))
    0))

;;; Exit protection

(defvar *jobs-exit-signaled* nil
  "True after orderly shell shutdown has signaled registered jobs.")

(defun job--owned-live-process-group-p (job shell-process-group)
  "True when JOB still owns a live group distinct from the shell group.
   Every spawned job uses its first process as its group leader. Requiring
   that identity and at least one live tracked process prevents shutdown
   from signaling an absent, malformed or already completed group."
  (let ((process-group (job-process-group job))
        (processes     (job-processes job)))
    (and (integerp process-group)
         (plusp process-group)
         (/= process-group shell-process-group)
         processes
         (= process-group (shell-process-pid (first processes)))
         (find-if (lambda (process)
                    (not (eq (shell-process-live-state process) ':done)))
                  processes)
         t)))

(defun job--signal-exit (job shell-process-group)
  "Send orderly exit signals to one registered JOB.
   Live groups receive SIGHUP. A stopped group then receives SIGCONT so it
   can act on the hangup instead of remaining suspended. In-process tasks
   are aborted separately because they are not members of the Unix group."
  (let ((status
          (handler-case
              (job-refresh job)
            (error ()
              (job-status job)))))
    (cond ((eq status ':done)
           (when (job-id job)
             (job-unregister job)))
          (t
           (when (job--owned-live-process-group-p job shell-process-group)
             (ignore-errors
               (job--signal-group job +job-sighup+ :missing-ok t))
             (when (eq status ':stopped)
               (ignore-errors
                 (job--signal-group job +sigcont+ :missing-ok t))))
           (ignore-errors
             (job--control-auxiliaries job ':abort)))))
  (values))

(defun jobs--signal-exit ()
  "Signal every registered live job once before orderly shell exit.
   Shutdown is best effort: one stale or failing job must not keep the shell
   alive, and only groups whose leader and live members are still tracked are
   addressed."
  (unless *jobs-exit-signaled*
    (setf *jobs-exit-signaled* t)
    (let ((shell-process-group (terminal-own-process-group)))
      (dolist (job (copy-list *jobs*))
        (handler-case
            (job--signal-exit job shell-process-group)
          (serious-condition () nil)))))
  (values))

(defun shell-quit (status)
  "Signal registered jobs and leave the shell with STATUS.
   A caller like cclsh -c ... | head can close standard output early,
   making the exit-time stream flush signal on the broken pipe; the error
   handler then falls back to a hard exit instead of dropping the dying
   image into an endless break loop."
  (terminal-finish-semantic-command status)
  (jobs--signal-exit)
  (quit status :error-handler (lambda (condition)
                                (declare (ignore condition))
                                (external-call "_exit" :int status))))

(defvar *jobs-exit-warned* nil
  "True right after the stopped jobs warning was printed, letting the
   directly following exit attempt proceed.")

(defvar *jobs-exit-confirmed* nil
  "Bound around each dispatched line and end-of-file to the warning
   state the attempt starts under, so exit twice in a row goes
   through while any other command rearms the warning.")

(defun jobs-stopped-p ()
  "True when the job table holds a stopped job."
  (and (find-if (lambda (job)
                  (eq (job-refresh job) ':stopped))
                *jobs*)
       t))

(defun jobs-exit-blocked-p ()
  "True when leaving the shell should be refused because of stopped
   jobs. Prints the warning and remembers it so the next attempt is
   allowed through."
  (cond ((not (jobs-stopped-p))
         nil)
        (*jobs-exit-confirmed*
         nil)
        (t
         (format *error-output* "~a~%"
                 (ansi-colorize "cclsh: there are stopped jobs" ':red))
         (force-output *error-output*)
         (setf *jobs-exit-warned* t)
         t)))


;;; Job builtins

(defun jobs-count ()
  "Number of jobs in the job table, reported to the prompt."
  (length *jobs*))

(defun job-find (spec)
  "Find a job by SPEC: NIL, % and %+ mean the current job, %- the
   previous one, an integer (with optional %) a job id, and any other
   %text the most current job whose command starts with text. Returns
   NIL when nothing matches."
  (let ((text (and spec (princ-to-string spec))))
    (cond ((or (null text)
               (string= text "%")
               (string= text "%+")
               (string= text "%%"))
           (job-current))
          ((string= text "%-")
           (second (jobs--ordered)))
          (t
           (let ((bare (if (and (plusp (length text))
                                (char= (char text 0) #\%))
                           (subseq text 1)
                           text)))
             (multiple-value-bind (id end)
                 (parse-integer bare :junk-allowed t)
               (if (and id (= end (length bare)))
                   (find id *jobs* :key #'job-id)
                   (find-if (lambda (job)
                              (let ((command (job-command job)))
                                (and (<= (length bare) (length command))
                                     (string= bare command
                                              :end2 (length bare)))))
                            (jobs--ordered)))))))))

(defun job-find-substring (spec)
  "Find the job named by SPEC, falling back to a command substring.
Standard job selectors and command prefixes retain JOB-FIND precedence."
  (or (job-find spec)
      (when spec
        (let* ((text (princ-to-string spec))
               (substring (if (and (plusp (length text))
                                   (char= (char text 0) #\%))
                              (subseq text 1)
                              text)))
          (and (plusp (length substring))
               (find-if (lambda (job)
                          (search substring (job-command job)))
                        (jobs--ordered)))))))

(defun job--complain (builtin spec)
  "Report a job lookup failure for BUILTIN and return its exit status."
  (format *error-output* "~a~%"
          (ansi-colorize (if spec
                             (format nil "~a: no such job: ~a" builtin spec)
                             (format nil "~a: no current job" builtin))
                         ':red))
  (force-output *error-output*)
  1)

(defun job--complain-finished (builtin job)
  "Report that the job BUILTIN was aimed at has already finished and
   return the exit status."
  (format *error-output* "~a~%"
          (ansi-colorize (format nil "~a: job [~d] has finished"
                                 builtin (job-id job))
                         ':red))
  (force-output *error-output*)
  1)

(defcommand jobs (&rest arguments)
  "List active jobs: id, the current + and previous - marks, status
   and command. jobs -l adds each job's process group id."
  (let ((show-pid (equal arguments '("-l"))))
    (cond ((and arguments (not show-pid))
           (format *error-output* "~a~%"
                   (ansi-colorize "jobs: the only supported option is -l"
                                  ':red))
           1)
          (t
           (dolist (job (sort (copy-list *jobs*) #'< :key #'job-id))
             (let ((status (job-refresh job)))
               (job-print job *standard-output* :show-pid show-pid)
               (setf (job-reported job) status)
               (when (eq status ':done)
                 (job-unregister job))))
           0))))

(defcommand disown (&optional spec)
  "Remove a job from shell management without signaling it. SPEC may be
   a job id, a standard job selector or any substring of the command;
   without it the current job is removed."
  (let ((job (job-find-substring spec)))
    (if job
        (progn
          (job-unregister job)
          0)
        (job--complain "disown" spec))))

(defcommand fg (&optional spec)
  "Resume a stopped or background job in the foreground. SPEC picks
   the job like jobs shows it: %1, 1, %- or a command prefix; without
   it the current job resumes."
  (let ((job (job-find spec)))
    (cond ((null job)
           (job--complain "fg" spec))
          ((eq (job-refresh job) ':done)
           (job--complain-finished "fg" job))
          (t
           (job-touch job)
           (format t "~a~%" (job-command job))
           (force-output)
           (job-run-foreground job :continue t)))))

(defcommand bg (&optional spec)
  "Resume a stopped job in the background, as if it had been launched
   with a trailing &. SPEC picks the job like fg."
  (let ((job (job-find spec)))
    (cond ((null job)
           (job--complain "bg" spec))
          ((eq (job-refresh job) ':done)
           (job--complain-finished "bg" job))
          ((eq (job-status job) ':running)
           (format *error-output* "~a~%"
                   (ansi-colorize
                    (format nil "bg: job [~d] is already running"
                            (job-id job))
                    ':red))
           (force-output *error-output*)
           0)
          (t
           (job-touch job)
           (job--continue-processes job)
           (setf (job-status job) ':running)
           (setf (job-reported job) ':running)
           (format t "[~d]~a ~a &~%"
                   (job-id job) (job-mark job) (job-command job))
           (force-output)
           0))))

(defun run (program &rest arguments)
  "Run PROGRAM with ARGUMENTS in the foreground and return its exit
   status. PROGRAM is a symbol or a string, arguments are stringified
   with PRINC-TO-STRING. When called synchronously by a builtin PIPE or
   CAPTURE stage, an external PROGRAM inherits that stage's standard
   descriptors and process group. Signals COMMAND-NOT-FOUND-ERROR."
  (let ((name  (command-designator-name program))
        (words (mapcar #'princ-to-string arguments)))
    (multiple-value-bind (kind target)
        (command-resolve-fresh name)
      (let ((*job-command-label* (format nil "~{~a~^ ~}" (cons name words))))
        (command-status-record
         (ecase kind
           (:builtin  (command-execute-builtin target words))
           (:external (if *run-external-executor*
                          (funcall *run-external-executor* target words)
                          (command-execute-external target words)))
           (:unknown  (error 'command-not-found-error :name name))))))))
