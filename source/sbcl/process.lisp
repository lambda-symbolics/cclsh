;;;; -- SBCL process backend --
;;;
;;; POSIX_SPAWN supplies atomic descriptor and process-group setup while
;;; SB-POSIX supplies the Unix descriptor and wait interfaces.

(in-package #:cclsh)

(cffi:defcfun ("posix_spawnattr_init" process--spawnattr-init) :int
  (attributes :pointer))
(cffi:defcfun ("posix_spawnattr_destroy" process--spawnattr-destroy) :int
  (attributes :pointer))
(cffi:defcfun ("posix_spawnattr_setpgroup" process--spawnattr-setpgroup) :int
  (attributes :pointer) (process-group :int))
(cffi:defcfun ("posix_spawnattr_setflags" process--spawnattr-setflags) :int
  (attributes :pointer) (flags :short))
(cffi:defcfun ("posix_spawn_file_actions_init" process--file-actions-init) :int
  (actions :pointer))
(cffi:defcfun ("posix_spawn_file_actions_destroy" process--file-actions-destroy)
    :int
  (actions :pointer))
(cffi:defcfun ("posix_spawn_file_actions_adddup2" process--file-actions-adddup2)
    :int
  (actions :pointer) (source :int) (target :int))
(cffi:defcfun ("posix_spawn_file_actions_addclose" process--file-actions-addclose)
    :int
  (actions :pointer) (descriptor :int))
(cffi:defcfun ("posix_spawn" process--posix-spawn) :int
  (pid :pointer) (program :string) (actions :pointer) (attributes :pointer)
  (arguments :pointer) (environment :pointer))

(defconstant +process-spawn-attribute-storage+ 1024)
(defconstant +process-spawn-file-actions-storage+ 1024)
(defconstant +process-spawn-setpgroup+ #x02)

(defconstant +process-o-read-only+ sb-posix:o-rdonly)
(defconstant +process-o-write-only+ sb-posix:o-wronly)
(defconstant +process-o-create+ sb-posix:o-creat)
(defconstant +process-o-truncate+ sb-posix:o-trunc)
(defconstant +process-o-append+ sb-posix:o-append)
(defconstant +process-f-duplicate+ sb-posix:f-dupfd)
(defconstant +process-wuntraced+ sb-posix:wuntraced)
(defconstant +process-sigkill+ sb-posix:sigkill)
(defconstant +process-sigint+ sb-posix:sigint)
(defconstant +process-sigquit+ sb-posix:sigquit)
(defconstant +process-sigpipe+ sb-posix:sigpipe)
(defconstant +process-sigtstp+ sb-posix:sigtstp)
(defconstant +process-sigttin+ sb-posix:sigttin)
(defconstant +process-sigttou+ sb-posix:sigttou)
(defconstant +process-eintr+ sb-posix:eintr)
(defconstant +process-echild+ sb-posix:echild)

(define-condition process-spawn-error (error)
  ((program :initarg :program :reader process-spawn-error-program)
   (operation :initarg :operation :reader process-spawn-error-operation)
   (code :initarg :code :reader process-spawn-error-code))
  (:report
   (lambda (condition stream)
     (format stream "Cannot ~a ~a: ~a"
             (process-spawn-error-operation condition)
             (process-spawn-error-program condition)
             (process--error-string (process-spawn-error-code condition))))))

(defstruct (shell-process (:constructor process--make (pid &key event)))
  "A child owned and reaped by CCLSH."
  (pid 0 :type integer :read-only t)
  (state ':running)
  (code nil)
  (generation 0)
  (event nil)
  (monitor nil)
  (lock (ccl:make-lock "cclsh child state") :read-only t))

(defun process--errno (condition)
  "Return CONDITION's POSIX errno value."
  (sb-posix:syscall-errno condition))

(defun process--error-string (code)
  "Return a portable diagnostic for errno CODE."
  (format nil "errno ~d" code))

(defun process--system-error (program operation condition)
  "Translate SB-POSIX CONDITION into CCLSH's process condition."
  (error 'process-spawn-error :program program :operation operation
         :code (process--errno condition)))

(defun fd-close (descriptor)
  "Close DESCRIPTOR, ignoring an already-closed descriptor."
  (ignore-errors (sb-posix:close descriptor))
  (values))

(defun fd-duplicate (descriptor &optional (minimum 3))
  "Duplicate DESCRIPTOR at or above MINIMUM."
  (handler-case
      (sb-posix:fcntl descriptor +process-f-duplicate+ minimum)
    (sb-posix:syscall-error (condition)
      (process--system-error descriptor "duplicate fd" condition))))

(defun fd-cloexec-pipe ()
  "Create a pipe for a CCLSH pipeline.

The POSIX spawn actions close the child-side descriptors after duplicating
them, so readers observe EOF once the final pipeline writer exits."
  (handler-case
      (sb-posix:pipe)
    (sb-posix:syscall-error (condition)
      (process--system-error "pipe" "create" condition))))

(defun fd-open-input (path)
  "Open PATH for byte input."
  (handler-case
      (sb-posix:open path +process-o-read-only+)
    (sb-posix:syscall-error (condition)
      (process--system-error path "open" condition))))

(defun fd-open-output (path &key append (mode #o666))
  "Open PATH for output, preserving MODE and APPEND semantics."
  (handler-case
      (sb-posix:open path
                     (logior +process-o-write-only+
                             +process-o-create+
                             (if append +process-o-append+
                                 +process-o-truncate+))
                     mode)
    (sb-posix:syscall-error (condition)
      (process--system-error path "open" condition))))

(defun fd-set-mode (descriptor mode)
  "Set DESCRIPTOR's mode to MODE."
  (handler-case
      (sb-posix:fchmod descriptor mode)
    (sb-posix:syscall-error (condition)
      (process--system-error descriptor "set permissions" condition)))
  (values))

(defun path-set-mode (path mode)
  "Set PATH's mode to MODE."
  (handler-case
      (sb-posix:chmod path mode)
    (sb-posix:syscall-error (condition)
      (process--system-error path "set permissions" condition)))
  (values))

(defun fd-input-stream (descriptor &key (auto-close t))
  "Make a UTF-8 character input stream for DESCRIPTOR."
  (sb-sys:make-fd-stream descriptor :input t :output nil
                         :element-type 'character :external-format :utf-8
                         :auto-close auto-close))

(defun fd-output-stream (descriptor &key (auto-close t))
  "Make a UTF-8 character output stream for DESCRIPTOR."
  (sb-sys:make-fd-stream descriptor :input nil :output t
                         :element-type 'character :external-format :utf-8
                         :auto-close auto-close))

(defun process--spawn-check (result program operation)
  "Signal a process error when a POSIX spawn operation returns RESULT."
  (unless (zerop result)
    (error 'process-spawn-error :program program :operation operation
           :code result)))

(defun process--call-with-c-strings (strings function)
  "Call FUNCTION with UTF-8 foreign string pointers for STRINGS.

All strings remain live until FUNCTION returns."
  (labels ((recur (remaining reversed)
             (if (null remaining)
                 (funcall function (nreverse reversed))
                 (cffi:with-foreign-string
                     (pointer (first remaining) :encoding :utf-8)
                   (recur (rest remaining) (cons pointer reversed))))))
    (recur strings nil)))

(defun process--call-with-c-vector (strings function)
  "Call FUNCTION with a null-terminated UTF-8 C vector for STRINGS."
  (process--call-with-c-strings
   strings
   (lambda (pointers)
     (cffi:with-foreign-object (vector :pointer (1+ (length pointers)))
       (loop for pointer in pointers
             for index from 0
             do (setf (cffi:mem-aref vector :pointer index) pointer))
       (setf (cffi:mem-aref vector :pointer (length pointers))
             (cffi:null-pointer))
       (funcall function vector)))))

(defun process--spawn-call (program arguments
                             &key environment process-group fd0 fd1 fd2 close-fds)
  "Perform POSIX_SPAWN and return its child pid.

The process group and standard descriptors are installed by POSIX_SPAWN before
the program executes.  Unlike SBCL:RUN-PROGRAM this has no parent-side SETPGID
race, which is necessary for pipelines and foreground job control."
  (cffi:with-foreign-object (attributes :uint64
                             (/ +process-spawn-attribute-storage+ 8))
    (cffi:with-foreign-object (actions :uint64
                               (/ +process-spawn-file-actions-storage+ 8))
      (cffi:with-foreign-object (pid :int)
        (let ((attributes-ready nil)
              (actions-ready nil))
          (unwind-protect
               (progn
                 (process--spawn-check
                  (process--spawnattr-init attributes)
                  program "initialize spawn attributes")
                 (setf attributes-ready t)
                 (process--spawn-check
                  (process--spawnattr-setpgroup attributes process-group)
                  program "set process group")
                 (process--spawn-check
                  (process--spawnattr-setflags attributes
                                               +process-spawn-setpgroup+)
                  program "enable process group")
                 (process--spawn-check
                  (process--file-actions-init actions)
                  program "initialize spawn file actions")
                 (setf actions-ready t)
                 (loop for source in (list fd0 fd1 fd2)
                       for target from 0
                       do (process--spawn-check
                           (process--file-actions-adddup2 actions source target)
                           program "install child file descriptor"))
                 ;; Closing the original descriptor copies is what allows a
                 ;; reader to observe EOF after the final writer exits.
                 (dolist (descriptor
                          (remove-duplicates
                           (append (list fd0 fd1 fd2) close-fds)))
                   (when (>= descriptor 3)
                     (process--spawn-check
                      (process--file-actions-addclose actions descriptor)
                      program "close child file descriptor")))
                 (process--call-with-c-vector
                  (cons program arguments)
                  (lambda (argument-vector)
                    (process--call-with-c-vector
                     environment
                     (lambda (environment-vector)
                       (process--spawn-check
                        (process--posix-spawn pid program actions attributes
                                              argument-vector environment-vector)
                        program "spawn")))))
                 (cffi:mem-ref pid :int))
            (when actions-ready
              (process--file-actions-destroy actions))
            (when attributes-ready
              (process--spawnattr-destroy attributes))))))))

(defun shell-process-spawn (program arguments
                            &key (process-group 0) (fd0 0) (fd1 1) (fd2 2)
                              (environment (environment-variables)) event close-fds)
  "Spawn PROGRAM with ARGUMENTS and return an unmonitored child record."
  (let ((program (namestring program)))
    (process--make
     (process--spawn-call program (mapcar #'string arguments)
                          :environment (mapcar #'string environment)
                          :process-group process-group :fd0 fd0 :fd1 fd1 :fd2 fd2
                          :close-fds close-fds)
     :event event)))

(defun process--wait-state (status)
  "Decode a Unix wait status into CCLSH's state and code."
  (cond ((= status #xffff) (values ':running nil))
        ((= (logand status #xff) #x7f)
         (values ':stopped (logand (ash status -8) #xff)))
        ((zerop (logand status #x7f))
         (values ':exited (logand (ash status -8) #xff)))
        (t (values ':signaled (logand status #x7f)))))

(defun process--publish-state (process state code)
  "Publish one child transition and notify its event semaphore."
  (let (event)
    (ccl:with-lock-grabbed ((shell-process-lock process))
      (setf (shell-process-state process) state
            (shell-process-code process) code)
      (incf (shell-process-generation process))
      (setf event (shell-process-event process)))
    (when event (ccl:signal-semaphore event)))
  (values))

(defun process--publish-lost-child (process)
  "Publish a conservative failure when another waiter consumed a child."
  (let (event)
    (ccl:with-lock-grabbed ((shell-process-lock process))
      (unless (member (shell-process-state process) '(:exited :signaled))
        (setf (shell-process-state process) ':exited
              (shell-process-code process) 127)
        (incf (shell-process-generation process))
        (setf event (shell-process-event process)))
    (when event (ccl:signal-semaphore event)))
  (values)))

(defun process--wait (process)
  "Monitor PROCESS with waitpid until it exits or is signaled."
  (loop
    (handler-case
        (multiple-value-bind (pid status)
            (sb-posix:waitpid (shell-process-pid process) +process-wuntraced+)
          (declare (ignore pid))
          (multiple-value-bind (state code) (process--wait-state status)
            (process--publish-state process state code)
            (when (member state '(:exited :signaled)) (return))))
      (sb-posix:syscall-error (condition)
        (let ((code (process--errno condition)))
          (cond ((= code +process-eintr+))
                ((= code +process-echild+)
                 (process--publish-lost-child process)
                 (return))
                (t
                 (process--publish-lost-child process)
                 (return)))))))
  (values))

(defun shell-process-start-monitor (process &optional event)
  "Start PROCESS's waitpid monitor once and return PROCESS."
  (ccl:with-lock-grabbed ((shell-process-lock process))
    (when event (setf (shell-process-event process) event))
    (unless (shell-process-monitor process)
      (setf (shell-process-monitor process)
            (ccl:process-run-function
             (format nil "cclsh child ~d" (shell-process-pid process))
             #'process--wait process))))
  process)

(defun shell-process-snapshot (process)
  "Return PROCESS's state, code and transition generation atomically."
  (ccl:with-lock-grabbed ((shell-process-lock process))
    (values (shell-process-state process) (shell-process-code process)
            (shell-process-generation process))))

(defun shell-process-status (process)
  "Return PROCESS's state, code and transition generation."
  (shell-process-snapshot process))

(defun shell-process-live-state (process)
  "Return :RUNNING, :STOPPED or :DONE for PROCESS."
  (case (nth-value 0 (shell-process-snapshot process))
    (:stopped ':stopped)
    ((:exited :signaled) ':done)
    (t ':running)))

(defun shell-process-exit-status (process)
  "Return PROCESS's shell status, or NIL while it remains live."
  (multiple-value-bind (state code) (shell-process-snapshot process)
    (case state
      (:exited code)
      (:signaled (+ 128 code))
      (t nil))))

(defun shell-process-kill (process signal &key group)
  "Send SIGNAL to PROCESS or its process group."
  (sb-posix:kill (if group (- (shell-process-pid process))
                    (shell-process-pid process))
                 signal))

(defun process-group-kill (process-group signal)
  "Send SIGNAL to PROCESS-GROUP and return success with errno."
  (handler-case
      (progn (sb-posix:kill (- process-group) signal) (values t 0))
    (sb-posix:syscall-error (condition)
      (values nil (process--errno condition)))))

(defun process--reap-synchronously (process)
  "Wait for an unmonitored PROCESS and publish its terminal state."
  (handler-case
      (multiple-value-bind (pid status)
          (sb-posix:waitpid (shell-process-pid process) 0)
        (declare (ignore pid))
        (multiple-value-bind (state code) (process--wait-state status)
          (process--publish-state process state code)))
    (sb-posix:syscall-error () (process--publish-lost-child process))))

(defun shell-process-kill-reap (process &optional (signal +process-sigkill+))
  "Ensure PROCESS is terminated and reaped, returning its shell status."
  (unless (eq (shell-process-live-state process) ':done)
    (ignore-errors (shell-process-kill process signal)))
  (let ((monitor (ccl:with-lock-grabbed ((shell-process-lock process))
                   (shell-process-monitor process))))
    (if monitor (ccl:join-process monitor) (process--reap-synchronously process)))
  (shell-process-exit-status process))
