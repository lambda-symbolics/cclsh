;;;; -- Child processes --
;;;
;;; The shell needs more control over child setup than CCL's
;;; RUN-PROGRAM exposes.  In particular, every stage of a pipeline must
;;; enter one process group before any child is reaped, and the shell's
;;; ignored signals must be reset in the child without changing them in
;;; the multithreaded parent.  POSIX-SPAWN provides both guarantees.

(in-package #:cclsh)

(defconstant +process-foreign-structure-size+ 512
  "Storage large enough for the host's spawn attributes and file actions.")

(defconstant +process-pointer-size+
  #+32-bit-target 4
  #-32-bit-target 8
  "Pointer size of the CCL target.")

(defconstant +process-o-cloexec+
  #+netbsd #x400000
  #-netbsd #x80000
  "O_CLOEXEC open flag for the host ABI.")

(defconstant +process-o-create+
  #+netbsd #x200
  #-netbsd #x40
  "O_CREAT open flag for the host ABI.")

(defconstant +process-o-truncate+
  #+netbsd #x400
  #-netbsd #x200
  "O_TRUNC open flag for the host ABI.")

(defconstant +process-o-append+
  #+netbsd #x08
  #-netbsd #x400
  "O_APPEND open flag for the host ABI.")

(defconstant +process-o-write-only+ 1
  "O_WRONLY open flag for the supported host ABIs.")

(defconstant +process-f-duplicate-cloexec+
  #+netbsd 12
  #-netbsd 1030
  "F_DUPFD_CLOEXEC fcntl operation for the host ABI.")

(defconstant +process-spawn-set-pgroup+ #x02
  "POSIX_SPAWN_SETPGROUP attribute flag.")

(defconstant +process-spawn-set-sigdefault+
  #+netbsd #x10
  #-netbsd #x04
  "POSIX_SPAWN_SETSIGDEF attribute flag.")

(defconstant +process-spawn-set-sigmask+
  #+netbsd #x20
  #-netbsd #x08
  "POSIX_SPAWN_SETSIGMASK attribute flag.")

(defconstant +process-wuntraced+ #x02
  "WUNTRACED waitpid flag for the supported host ABIs.")

(defconstant +process-wcontinued+
  #+netbsd #x10
  #-netbsd #x08
  "WCONTINUED waitpid flag for the host ABI.")

(defconstant +process-sigint+ 2
  "SIGINT signal number for the supported host ABIs.")

(defconstant +process-sigquit+ 3
  "SIGQUIT signal number for the supported host ABIs.")

(defconstant +process-sigkill+ 9
  "SIGKILL signal number for the supported host ABIs.")

(defconstant +process-sigpipe+ 13
  "SIGPIPE signal number for the supported host ABIs.")

(defconstant +process-sigtstp+
  #+netbsd 18
  #-netbsd 20
  "SIGTSTP signal number for the host ABI.")

(defconstant +process-sigttin+ 21
  "SIGTTIN signal number for the supported host ABIs.")

(defconstant +process-sigttou+ 22
  "SIGTTOU signal number for the supported host ABIs.")

(defconstant +process-eintr+ 4
  "EINTR error number for the supported host ABIs.")

(defconstant +process-esrch+ 3
  "ESRCH error number for the supported host ABIs.")

(defconstant +process-echild+ 10
  "ECHILD error number for the supported host ABIs.")

(defconstant +process-sigset-size+
  #+netbsd 16
  #-netbsd 128
  "Size in bytes of the host's sigset_t.")

(define-condition process-spawn-error (error)
  ((program
    :initarg :program
    :reader process-spawn-error-program)
   (operation
    :initarg :operation
    :reader process-spawn-error-operation)
   (code
    :initarg :code
    :reader process-spawn-error-code))
  (:documentation "Signaled when process or descriptor setup fails.")
  (:report
   (lambda (condition stream)
     (format stream "Cannot ~a ~a: ~a"
             (process-spawn-error-operation condition)
             (process-spawn-error-program condition)
             (process--error-string
              (process-spawn-error-code condition))))))

(defstruct (shell-process
            (:constructor process--make
                (pid &key event)))
  "A child owned and reaped by CCLSH.

STATE is :RUNNING, :STOPPED, :EXITED or :SIGNALED. CODE is NIL while
running, the stop signal while stopped, and the raw exit status or
terminating signal after death. GENERATION increases for every published
transition. EVENT is signaled after each transition. MONITOR and LOCK are
private lifecycle machinery."
  (pid 0 :type integer :read-only t)
  (state ':running)
  (code nil)
  (generation 0)
  (event nil)
  (monitor nil)
  (lock (ccl:make-lock "cclsh child state") :read-only t))


;;; File descriptors

(defun process--errno ()
  "Return the calling thread's current libc errno."
  (ccl::get-errno))

(defun process--error-string (code)
  "Return libc's description of error CODE."
  (let ((pointer (external-call "strerror" :int code :address)))
    (if (ccl:%null-ptr-p pointer)
        (format nil "system error ~d" code)
        (ccl::%get-utf-8-cstring pointer))))

(defun process--system-error (program operation code)
  "Signal a PROCESS-SPAWN-ERROR for a failed libc operation."
  (error 'process-spawn-error
         :program program
         :operation operation
         :code code))

(defun process--call-retrying-interrupts (function)
  "Call FUNCTION until it succeeds or fails with an error other than EINTR.
   Return the integer result and the final errno, which is zero on success."
  (loop
    (let ((result (funcall function)))
      (unless (minusp result)
        (return (values result 0)))
      (let ((code (process--errno)))
        (unless (= code +process-eintr+)
          (return (values result code)))))))

(defun fd-close (descriptor)
  "Close DESCRIPTOR.  NIL and negative descriptors are harmless."
  (when (and descriptor (not (minusp descriptor)))
    (external-call "close" :int descriptor :int))
  (values))

(defun fd-duplicate (descriptor &optional (minimum 3))
  "Duplicate DESCRIPTOR at or above MINIMUM with close-on-exec set."
  (multiple-value-bind (duplicate code)
      (process--call-retrying-interrupts
       (lambda ()
         (external-call "fcntl"
                        :int descriptor
                        :int +process-f-duplicate-cloexec+
                        :int minimum
                        :int)))
    (if (minusp duplicate)
        (process--system-error descriptor "duplicate fd" code)
        duplicate)))

(defun fd-cloexec-pipe ()
  "Create a close-on-exec pipe and return its read and write fds."
  (ccl:%stack-block ((descriptors 8))
    (multiple-value-bind (result code)
        (process--call-retrying-interrupts
         (lambda ()
           (external-call "pipe2"
                          :address descriptors
                          :int +process-o-cloexec+
                          :int)))
      (unless (zerop result)
        (process--system-error "pipe" "create" code)))
    (values (ccl:%get-signed-long descriptors 0)
            (ccl:%get-signed-long descriptors 4))))

(defun fd-open-input (path)
  "Open PATH for input with close-on-exec and return its descriptor."
  (let ((path (namestring path)))
    (ccl::with-utf-8-cstr (encoded path)
      (multiple-value-bind (descriptor code)
          (process--call-retrying-interrupts
           (lambda ()
             (external-call "open"
                            :address encoded
                            :int +process-o-cloexec+
                            :int)))
        (if (minusp descriptor)
            (process--system-error path "open" code)
            descriptor)))))

(defun fd-open-output (path &key append (mode #o666))
  "Open PATH for output with close-on-exec and return its descriptor.
Existing files are truncated unless APPEND is true. MODE supplies the
permissions for a newly created file, before the process umask."
  (let ((path (namestring path))
        (flags (logior +process-o-write-only+
                       +process-o-create+
                       +process-o-cloexec+
                       (if append
                           +process-o-append+
                           +process-o-truncate+))))
    (ccl::with-utf-8-cstr (encoded path)
      (multiple-value-bind (descriptor code)
          (process--call-retrying-interrupts
           (lambda ()
             (external-call "open"
                            :address encoded
                            :int flags
                            :unsigned-int mode
                            :int)))
        (if (minusp descriptor)
            (process--system-error path "open" code)
            descriptor)))))

(defun fd-set-mode (descriptor mode)
  "Set DESCRIPTOR permissions to MODE."
  (multiple-value-bind (result code)
      (process--call-retrying-interrupts
       (lambda ()
         (external-call "fchmod"
                        :int descriptor
                        :unsigned-int mode
                        :int)))
    (unless (zerop result)
      (process--system-error descriptor "set permissions" code)))
  (values))

(defun path-set-mode (path mode)
  "Set PATH permissions to MODE."
  (let ((path (namestring path)))
    (ccl::with-utf-8-cstr (encoded path)
      (multiple-value-bind (result code)
          (process--call-retrying-interrupts
           (lambda ()
             (external-call "chmod"
                            :address encoded
                            :unsigned-int mode
                            :int)))
        (unless (zerop result)
          (process--system-error path "set permissions" code)))))
  (values))

(defun fd-input-stream (descriptor &key (auto-close t))
  "Wrap DESCRIPTOR in a UTF-8 character input stream."
  (ccl::make-fd-stream descriptor
                       :direction ':input
                       :element-type 'character
                       :encoding ':utf-8
                       :line-termination ':unix
                       :sharing ':lock
                       :auto-close auto-close))

(defun fd-output-stream (descriptor &key (auto-close t))
  "Wrap DESCRIPTOR in a UTF-8 character output stream."
  (ccl::make-fd-stream descriptor
                       :direction ':output
                       :element-type 'character
                       :encoding ':utf-8
                       :line-termination ':unix
                       :sharing ':lock
                       :auto-close auto-close))


;;; UTF-8 C vectors

(defun process--encode-string (string)
  "Encode STRING as UTF-8, rejecting the NUL that terminates C strings."
  (when (find #\null string)
    (error "A child argument or environment entry contains NUL"))
  (ccl:encode-string-to-octets string :external-format ':utf-8))

(defun process--call-with-c-vector (strings function)
  "Call FUNCTION with a null-terminated char ** for UTF-8 STRINGS."
  (let* ((octet-vectors (mapcar #'process--encode-string strings))
         (byte-count (reduce #'+ octet-vectors
                             :key (lambda (octets)
                                    (1+ (length octets)))
                             :initial-value 0))
         (pointer-count (1+ (length strings))))
    (ccl:%stack-block ((pointers (* pointer-count
                                     +process-pointer-size+))
                       (bytes (max 1 byte-count)))
      (let ((offset 0))
        (loop for octets in octet-vectors
              for index from 0
              for pointer = (ccl:%inc-ptr bytes offset)
              do (setf (ccl:%get-ptr pointers
                                     (* index +process-pointer-size+))
                       pointer)
                 (loop for octet across octets
                       for byte-index from offset
                       do (setf (ccl:%get-unsigned-byte bytes byte-index)
                                octet))
                 (incf offset (length octets))
                 (setf (ccl:%get-unsigned-byte bytes offset) 0)
                 (incf offset))
        (setf (ccl:%get-ptr pointers
                            (* (length strings) +process-pointer-size+))
              (ccl:%int-to-ptr 0))
        (funcall function pointers)))))


;;; POSIX spawn

(defun process--spawn-check (result program operation)
  "Signal when a POSIX spawn setup operation returned an error."
  (unless (zerop result)
    (process--system-error program operation result))
  (values))

(defun process--spawn-signals (attributes program)
  "Give the child ordinary shell signal defaults and an empty mask."
  (ccl:%stack-block ((defaults +process-sigset-size+)
                     (mask +process-sigset-size+))
    (process--spawn-check
     (external-call "sigemptyset" :address defaults :int)
     program "clear child signal defaults")
    (dolist (signal (list +process-sigint+
                          +process-sigquit+
                          +process-sigpipe+
                          +process-sigtstp+
                          +process-sigttin+
                          +process-sigttou+))
      (process--spawn-check
       (external-call "sigaddset"
                      :address defaults
                      :int signal
                      :int)
       program "set child signal default"))
    (process--spawn-check
     (external-call "posix_spawnattr_setsigdefault"
                    :address attributes
                    :address defaults
                    :int)
     program "set child signal defaults")
    (process--spawn-check
     (external-call "sigemptyset" :address mask :int)
     program "clear child signal mask")
    (process--spawn-check
     (external-call "posix_spawnattr_setsigmask"
                    :address attributes
                    :address mask
                    :int)
     program "set child signal mask")))

(defun process--configure-spawn-attributes (attributes process-group program)
  "Configure initialized ATTRIBUTES for a child in PROCESS-GROUP."
  (process--spawn-check
   (external-call "posix_spawnattr_setpgroup"
                  :address attributes
                  :int process-group
                  :int)
   program "set child process group")
  (process--spawn-signals attributes program)
  (process--spawn-check
   (external-call "posix_spawnattr_setflags"
                  :address attributes
                  :short (logior +process-spawn-set-pgroup+
                                 +process-spawn-set-sigdefault+
                                 +process-spawn-set-sigmask+)
                  :int)
   program "enable spawn attributes"))

(defun process--configure-close-actions (actions program)
  "Configure ACTIONS to close nonstandard descriptors in the child."
  #-netbsd
  (process--spawn-check
   (external-call "posix_spawn_file_actions_addclosefrom_np"
                  :address actions
                  :int 3
                  :int)
   program "close child file descriptors")
  #+netbsd
  (loop for descriptor from 3 below (external-call "getdtablesize" :int)
        do (process--spawn-check
            (external-call "posix_spawn_file_actions_addclose"
                           :address actions
                           :int descriptor
                           :int)
            program "close child file descriptor"))
  (values))

(defun process--configure-file-actions (actions &key fd0 fd1 fd2 program)
  "Configure initialized ACTIONS to install and contain the standard fds."
  (loop for source in (list fd0 fd1 fd2)
        for target from 0
        do (process--spawn-check
            (external-call "posix_spawn_file_actions_adddup2"
                           :address actions
                           :int source
                           :int target
                           :int)
            program "install child file descriptor"))
  (process--configure-close-actions actions program))

(defun process--spawn-call (program arguments
                            &key environment process-group fd0 fd1 fd2)
  "Perform POSIX-SPAWN and return the new pid."
  (ccl:%stack-block ((attributes +process-foreign-structure-size+)
                     (actions +process-foreign-structure-size+)
                     (pid 4))
    (let ((attributes-ready nil)
          (actions-ready nil))
      (unwind-protect
          (progn
            (process--spawn-check
             (external-call "posix_spawnattr_init"
                            :address attributes
                            :int)
             program "initialize spawn attributes")
            (setf attributes-ready t)
            (process--configure-spawn-attributes
             attributes process-group program)
            (process--spawn-check
             (external-call "posix_spawn_file_actions_init"
                            :address actions
                            :int)
             program "initialize spawn file actions")
            (setf actions-ready t)
            (process--configure-file-actions
             actions :fd0 fd0 :fd1 fd1 :fd2 fd2 :program program)
            (process--call-with-c-vector
             (cons program arguments)
             (lambda (argv)
               (process--call-with-c-vector
                environment
                (lambda (envp)
                  (process--spawn-check
                   (external-call "posix_spawn"
                                  :address pid
                                  :address (ccl:%get-ptr argv 0)
                                  :address actions
                                  :address attributes
                                  :address argv
                                  :address envp
                                  :int)
                   program "spawn")))))
            (ccl:%get-signed-long pid 0))
        (when actions-ready
          (external-call "posix_spawn_file_actions_destroy"
                         :address actions
                         :int))
        (when attributes-ready
          (external-call "posix_spawnattr_destroy"
                         :address attributes
                         :int))))))

(defun shell-process-spawn (program arguments
                            &key
                              (process-group 0)
                              (fd0 0)
                              (fd1 1)
                              (fd2 2)
                              (environment (environment-variables))
                              event)
  "Spawn PROGRAM with UTF-8 ARGUMENTS and ENVIRONMENT.

PROCESS-GROUP zero makes the child a group leader; a positive value
joins that existing group.  FD0, FD1 and FD2 become its standard file
descriptors.  The caller retains ownership of those descriptors.

The process is deliberately returned unmonitored.  Pipelines must
spawn every stage before calling SHELL-PROCESS-START-MONITOR, keeping
a fast first-stage leader unreaped while later stages join its group."
  (let ((program (namestring program)))
    (process--make
     (process--spawn-call program
                          (mapcar #'string arguments)
                          :environment (mapcar #'string environment)
                          :process-group process-group
                          :fd0 fd0
                          :fd1 fd1
                          :fd2 fd2)
     :event event)))


;;; Waiting and lifecycle

(defun process--wait-state (status)
  "Decode waitpid STATUS into a process state and code."
  (cond ((= status #xffff)
         (values ':running nil))
        ((= (logand status #xff) #x7f)
         (values ':stopped (logand (ash status -8) #xff)))
        ((zerop (logand status #x7f))
         (values ':exited (logand (ash status -8) #xff)))
        (t
         (values ':signaled (logand status #x7f)))))

(defun process--publish-state (process state code)
  "Publish one child transition and notify PROCESS's event."
  (let (event)
    (ccl:with-lock-grabbed ((shell-process-lock process))
      (setf (shell-process-state process) state)
      (setf (shell-process-code process) code)
      (incf (shell-process-generation process))
      (setf event (shell-process-event process)))
    (when event
      (ccl:signal-semaphore event)))
  (values))

(defun process--publish-lost-child (process)
  "Publish status 127 if no other waiter already finalized PROCESS."
  (let (event)
    (ccl:with-lock-grabbed ((shell-process-lock process))
      (unless (member (shell-process-state process) '(:exited :signaled))
        (setf (shell-process-state process) ':exited)
        (setf (shell-process-code process) 127)
        (incf (shell-process-generation process))
        (setf event (shell-process-event process))))
    (when event
      (ccl:signal-semaphore event)))
  (values))

(defun process--recover-monitor-error (process)
  "Kill and reap PROCESS after its monitor receives a waitpid error."
  (ignore-errors
    (external-call "kill"
                   :int (shell-process-pid process)
                   :int +process-sigkill+
                   :int))
  (ccl:%stack-block ((status-pointer 4))
    (loop
      (let ((result
              (external-call "waitpid"
                             :int (shell-process-pid process)
                             :address status-pointer
                             :int 0
                             :int)))
        (cond ((plusp result)
               (multiple-value-bind (state code)
                   (process--wait-state
                    (ccl:%get-signed-long status-pointer 0))
                 (process--publish-state process state code))
               (return))
              ((= (process--errno) +process-eintr+))
              (t
               (process--publish-lost-child process)
               (return))))))
  (values))

(defun process--wait (process)
  "Monitor PROCESS with blocking waitpid until it dies."
  (ccl:%stack-block ((status-pointer 4))
    (loop
      (let ((result
              (external-call "waitpid"
                             :int (shell-process-pid process)
                             :address status-pointer
                             :int (logior +process-wuntraced+
                                          +process-wcontinued+)
                             :int)))
        (cond ((plusp result)
               (multiple-value-bind (state code)
                   (process--wait-state
                    (ccl:%get-signed-long status-pointer 0))
                 (process--publish-state process state code)
                 (when (member state '(:exited :signaled))
                   (return))))
              ((= (process--errno) +process-eintr+))
              ((= (process--errno) +process-echild+)
               ;; Synchronous failure cleanup may have won a race with
               ;; monitor startup. Preserve its final publication; if
               ;; no waiter published one, fail closed instead of
               ;; leaving the job running forever.
               (process--publish-lost-child process)
               (return))
              (t
               (process--recover-monitor-error process)
               (return))))))
  (values))

(defun shell-process-start-monitor (process &optional event)
  "Start PROCESS's sole waitpid monitor and return PROCESS.

When EVENT is supplied it replaces the event installed at spawn time.
The event is signaled after stop, continue, exit and signal transitions."
  (ccl:with-lock-grabbed ((shell-process-lock process))
    (when event
      (setf (shell-process-event process) event))
    (unless (shell-process-monitor process)
      (setf (shell-process-monitor process)
            (ccl:process-run-function
              (format nil "cclsh child ~d" (shell-process-pid process))
              #'process--wait process))))
  process)

(defun shell-process-snapshot (process)
  "Return PROCESS's state, raw code and transition generation atomically."
  (ccl:with-lock-grabbed ((shell-process-lock process))
    (values (shell-process-state process)
            (shell-process-code process)
            (shell-process-generation process))))

(defun shell-process-status (process)
  "Return PROCESS's state, raw code and transition generation."
  (shell-process-snapshot process))

(defun shell-process-live-state (process)
  "Return :RUNNING, :STOPPED or :DONE for PROCESS."
  (multiple-value-bind (state code)
      (shell-process-snapshot process)
    (declare (ignore code))
    (case state
      (:stopped ':stopped)
      ((:exited :signaled) ':done)
      (t ':running))))

(defun shell-process-exit-status (process)
  "Return PROCESS's shell status, or NIL while it is still live."
  (multiple-value-bind (state code)
      (shell-process-snapshot process)
    (case state
      (:exited code)
      (:signaled (+ 128 code))
      (t nil))))

(defun shell-process-kill (process signal &key group)
  "Send SIGNAL to PROCESS, or to its process GROUP when GROUP is true."
  (external-call "kill"
                 :int (if group
                          (- (shell-process-pid process))
                          (shell-process-pid process))
                 :int signal
                 :int))

(defun process-group-kill (process-group signal)
  "Send SIGNAL to PROCESS-GROUP. Return success and final errno."
  (loop
    (when (zerop (external-call "kill"
                                :int (- process-group)
                                :int signal
                                :int))
      (return (values t 0)))
    (let ((code (process--errno)))
      (unless (= code +process-eintr+)
        (return (values nil code))))))

(defun process--reap-synchronously (process)
  "Wait for an unmonitored PROCESS to terminate and publish its status."
  (ccl:%stack-block ((status-pointer 4))
    (loop
      (let ((result
              (external-call "waitpid"
                             :int (shell-process-pid process)
                             :address status-pointer
                             :int 0
                             :int)))
        (cond ((plusp result)
               (multiple-value-bind (state code)
                   (process--wait-state
                    (ccl:%get-signed-long status-pointer 0))
                 (process--publish-state process state code))
               (return))
              ((= (process--errno) +process-eintr+))
              ((= (process--errno) +process-echild+)
               (return))
              (t
               (process--publish-state process ':exited 127)
               (return)))))))

(defun shell-process-kill-reap (process
                                &optional (signal +process-sigkill+))
  "Kill PROCESS if needed and synchronously ensure that it is reaped."
  (multiple-value-bind (state code)
      (shell-process-snapshot process)
    (declare (ignore code))
    (unless (member state '(:exited :signaled))
      (shell-process-kill process signal)))
  (let ((monitor
          (ccl:with-lock-grabbed ((shell-process-lock process))
            (shell-process-monitor process))))
    (if monitor
        (ccl:join-process monitor)
        (process--reap-synchronously process)))
  (shell-process-exit-status process))
