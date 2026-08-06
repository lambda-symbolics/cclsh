;;;; -- Terminal control --
;;;
;;; In-process terminal control through libc: raw mode with tcgetattr
;;; and tcsetattr, size with the TIOCGWINSZ ioctl, and foreground
;;; process group handling with tcsetpgrp. CCL runs external programs
;;; in their own process groups, so the shell must hand them the
;;; terminal for the duration of a foreground command and take it back
;;; afterwards. The byte offsets below follow the host termios layout.

(in-package #:cclsh)

(defconstant +termios-size+ 128
  "Buffer size comfortably larger than the supported struct termios layouts.")

(defconstant +termios-lflag-offset+
  12
  "Byte offset of c_lflag in the supported struct termios layouts.")

(defconstant +termios-vtime-offset+
  #+netbsd 33
  #-netbsd 22
  "Byte offset of c_cc[VTIME] in the host struct termios.")

(defconstant +termios-vmin-offset+
  #+netbsd 32
  #-netbsd 23
  "Byte offset of c_cc[VMIN] in the host struct termios.")

(defconstant +termios-raw-lflag-mask+
  #+netbsd #x188
  #-netbsd #x0b
  "The host ISIG, ICANON and ECHO bits of c_lflag.")

(defconstant +tcsanow+ 0
  "tcsetattr optional action: apply immediately.")

(defconstant +winsize-ioctl+
  #+netbsd #x40087468
  #-netbsd #x5413
  "The host TIOCGWINSZ ioctl request.")

(defconstant +sigcont+
  #+netbsd 19
  #-netbsd 18
  "Host signal number of SIGCONT.")

(defconstant +sigtstp+
  #+netbsd 18
  #-netbsd 20
  "Host signal number of SIGTSTP.")

(defconstant +sigttin+ 21
  "Signal number of SIGTTIN.")

(defconstant +sigttou+ 22
  "Signal number of SIGTTOU.")

(defconstant +terminal-sigset-size+
  #+netbsd 16
  #-netbsd 128
  "Size in bytes of the host's sigset_t.")

(defconstant +terminal-sig-block+
  #+netbsd 1
  #-netbsd 0
  "PTHREAD_SIGMASK operation which adds signals to the caller's mask.")

(defconstant +terminal-sig-setmask+
  #+netbsd 3
  #-netbsd 2
  "PTHREAD_SIGMASK operation which restores the calling thread's signal mask.")

(defconstant +terminal-eintr+ 4
  "EINTR for the supported host ABIs.")

(defconstant +terminal-esrch+ 3
  "ESRCH for the supported host ABIs.")

(defconstant +terminal-eperm+ 1
  "EPERM for the supported host ABIs.")

(define-condition terminal-control-error (error)
  ((operation
    :initarg :operation
    :reader terminal-control-error-operation)
   (process-group
    :initarg :process-group
    :reader terminal-control-error-process-group)
   (code
    :initarg :code
    :reader terminal-control-error-code))
  (:documentation "Signaled when a foreground terminal handoff fails.")
  (:report
   (lambda (condition stream)
     (format stream "cannot ~a process group ~d: ~a"
             (terminal-control-error-operation condition)
             (terminal-control-error-process-group condition)
             (let ((pointer
                     (external-call
                      "strerror"
                      :int (terminal-control-error-code condition)
                      :address)))
               (if (ccl:%null-ptr-p pointer)
                   (format nil "system error ~d"
                           (terminal-control-error-code condition))
                   (ccl::%get-utf-8-cstring pointer)))))))

(define-condition terminal-attributes-error (error)
  ((operation
    :initarg :operation
    :reader terminal-attributes-error-operation)
   (code
    :initarg :code
    :reader terminal-attributes-error-code))
  (:documentation "Signaled when terminal attributes cannot be handled.")
  (:report
   (lambda (condition stream)
     (let* ((code (terminal-attributes-error-code condition))
            (pointer (external-call "strerror" :int code :address)))
       (format stream "cannot ~a terminal attributes: ~a"
               (terminal-attributes-error-operation condition)
               (if (ccl:%null-ptr-p pointer)
                   (format nil "system error ~d" code)
                   (ccl::%get-utf-8-cstring pointer)))))))

(defvar *terminal-control-signals-active* nil
  "True while CCLSH owns the host process's terminal signal policy.

Shell entry points bind this after installing their persistent dispositions.
Library callers leave it NIL, so individual terminal operations protect
themselves without changing the embedding process's signal policy.")

(defun terminal--signal-mask-check (result process-group operation)
  "Require a successful signal-set or PTHREAD_SIGMASK RESULT."
  (unless (zerop result)
    (error 'terminal-control-error
           :operation operation
           :process-group process-group
           :code (if (minusp result) (ccl::get-errno) result)))
  (values))

(defun terminal--call-with-sigttou-safe (process-group function)
  "Call FUNCTION without letting terminal control stop the current process.

The shell entry points already ignore SIGTTOU. In library use, block it only
in the calling thread and restore the exact prior mask after FUNCTION."
  (if *terminal-control-signals-active*
      (funcall function)
      (ccl:%stack-block ((signals +terminal-sigset-size+)
                         (old-signals +terminal-sigset-size+))
        (let ((mask-installed nil))
          (unwind-protect
              (progn
                (terminal--signal-mask-check
                 (external-call "sigemptyset" :address signals :int)
                 process-group "prepare SIGTTOU blocking for")
                (terminal--signal-mask-check
                 (external-call "sigaddset"
                                :address signals
                                :int +sigttou+
                                :int)
                 process-group "prepare SIGTTOU blocking for")
                (ccl:without-interrupts
                  (terminal--signal-mask-check
                   (external-call "pthread_sigmask"
                                  :int +terminal-sig-block+
                                  :address signals
                                  :address old-signals
                                  :int)
                   process-group "block SIGTTOU while controlling")
                  (setf mask-installed t))
                (funcall function))
            (when mask-installed
              (ccl:without-interrupts
                (terminal--signal-mask-check
                 (external-call "pthread_sigmask"
                                :int +terminal-sig-setmask+
                                :address old-signals
                                :address (ccl:%null-ptr)
                                :int)
                 process-group
                 "restore the signal mask after controlling"))))))))

(defvar *terminal-saved-termios* nil
  "Saved termios bytes, restored after raw line editing.")

(defvar *terminal-shell-attributes* nil
  "The terminal attributes of an interactive session at startup, the
   known good state reapplied when a stopped job leaves the terminal
   in whatever mode it was using.")

;;; Raw mode

(defun terminal-tty-p ()
  "True when standard input is an interactive terminal."
  (= 1 (external-call "isatty" :int 0 :int)))

(defun terminal-output-tty-p ()
  "True when terminal presentation is enabled and output is a terminal."
  (and *presentation-enabled*
       (= 1 (external-call "isatty" :int 1 :int))))

(defun terminal--get-termios (pointer)
  "Fill POINTER with terminal attributes. Return success and errno."
  (loop
    (when (zerop (external-call "tcgetattr"
                                :int 0
                                :address pointer
                                :int))
      (return (values t 0)))
    (let ((code (ccl::get-errno)))
      (unless (= code +terminal-eintr+)
        (return (values nil code))))))

(defun terminal--set-termios (pointer)
  "Apply attributes at POINTER. Return success and final errno."
  (terminal--call-with-sigttou-safe
   (external-call "getpgrp" :int)
   (lambda ()
     (loop
       (when (zerop (external-call "tcsetattr"
                                   :int 0
                                   :int +tcsanow+
                                   :address pointer
                                   :int))
         (return (values t 0)))
       (let ((code (ccl::get-errno)))
         (unless (= code +terminal-eintr+)
           (return (values nil code))))))))

(defun terminal-raw ()
  "Switch the terminal to character-at-a-time input without echo or
   signal generation. Returns true when the switch succeeded."
  (ccl:%stack-block ((pointer +termios-size+))
    (when (terminal--get-termios pointer)
      (let ((saved (make-array +termios-size+ :element-type '(unsigned-byte 8))))
        (dotimes (index +termios-size+)
          (setf (aref saved index) (ccl:%get-unsigned-byte pointer index)))
        (setf *terminal-saved-termios* saved))
      (setf (ccl:%get-unsigned-long pointer +termios-lflag-offset+)
            (logandc2 (ccl:%get-unsigned-long pointer +termios-lflag-offset+)
                      +termios-raw-lflag-mask+))
      (setf (ccl:%get-unsigned-byte pointer +termios-vtime-offset+) 0)
      (setf (ccl:%get-unsigned-byte pointer +termios-vmin-offset+) 1)
      (terminal--set-termios pointer))))

(defun terminal-restore ()
  "Restore the terminal state saved by TERMINAL-RAW."
  (when *terminal-saved-termios*
    (unwind-protect
        (ccl:%stack-block ((pointer +termios-size+))
          (dotimes (index +termios-size+)
            (setf (ccl:%get-unsigned-byte pointer index)
                  (aref *terminal-saved-termios* index)))
          (multiple-value-bind (success code)
              (terminal--set-termios pointer)
            (unless success
              (error 'terminal-attributes-error
                     :operation "restore"
                     :code code))))
      (setf *terminal-saved-termios* nil)))
  (values))

(defun terminal-attributes ()
  "Snapshot the current terminal attributes. Returns a byte vector for
   TERMINAL-ATTRIBUTES-APPLY, or NIL when they cannot be read."
  (ccl:%stack-block ((pointer +termios-size+))
    (when (terminal--get-termios pointer)
      (let ((saved (make-array +termios-size+ :element-type '(unsigned-byte 8))))
        (dotimes (index +termios-size+)
          (setf (aref saved index) (ccl:%get-unsigned-byte pointer index)))
        saved))))

(defun terminal-attributes-checked ()
  "Snapshot terminal attributes or signal TERMINAL-ATTRIBUTES-ERROR."
  (ccl:%stack-block ((pointer +termios-size+))
    (multiple-value-bind (success code)
        (terminal--get-termios pointer)
      (unless success
        (error 'terminal-attributes-error
               :operation "read"
               :code code)))
    (let ((saved (make-array +termios-size+
                             :element-type '(unsigned-byte 8))))
      (dotimes (index +termios-size+)
        (setf (aref saved index) (ccl:%get-unsigned-byte pointer index)))
      saved)))

(defun terminal-attributes-apply (attributes)
  "Apply an ATTRIBUTES snapshot taken by TERMINAL-ATTRIBUTES. Does
   nothing when ATTRIBUTES is NIL."
  (when attributes
    (ccl:%stack-block ((pointer +termios-size+))
      (dotimes (index +termios-size+)
        (setf (ccl:%get-unsigned-byte pointer index) (aref attributes index)))
      (multiple-value-bind (success code)
          (terminal--set-termios pointer)
        (unless success
          (error 'terminal-attributes-error
                 :operation "apply"
                 :code code)))))
  (values))

(defun terminal-shell-attributes-save ()
  "Remember the startup terminal attributes of an interactive session
   in *TERMINAL-SHELL-ATTRIBUTES*."
  (setf *terminal-shell-attributes* (terminal-attributes))
  (values))

(defun terminal-size ()
  "Return the terminal dimensions as (values rows columns).
   Falls back to 24 by 80 when the size cannot be determined."
  (ccl:%stack-block ((pointer 8))
    (if (zerop (external-call "ioctl" :int 0 :unsigned-long +winsize-ioctl+
                              :address pointer :int))
        (let ((rows    (ccl:%get-unsigned-word pointer 0))
              (columns (ccl:%get-unsigned-word pointer 2)))
          (values (if (plusp rows) rows 24)
                  (if (plusp columns) columns 80)))
        (values 24 80))))


;;; Foreground process groups

(defun terminal--signal-disposition (signal disposition)
  "Set the handler of SIGNAL to DISPOSITION: 0 for the default action,
   1 to ignore it."
  (external-call "signal" :int signal :address (ccl:%int-to-ptr disposition)
                 :address)
  (values))

(defun terminal-signals-setup ()
  "Ignore the terminal stop signals so handing the terminal to child
   process groups and taking it back never stops the shell. SIGTSTP
   keeps its default action: children inherit signal dispositions
   across exec, and an inherited ignore would make Ctrl-Z dead in
   every program the shell runs."
  (terminal--signal-disposition +sigttou+ 1)
  (terminal--signal-disposition +sigttin+ 1)
  (values))

(defmacro with-terminal-control-signals (&body body)
  "Run BODY under the terminal signal policy of a CCLSH entry point."
  `(progn
     (terminal-signals-setup)
     (let ((*terminal-control-signals-active* t))
       ,@body)))

(defun terminal-own-process-group ()
  "Return the shell's own process group id."
  (external-call "getpgrp" :int))

(defun terminal-foreground (process-group)
  "Make PROCESS-GROUP the terminal's foreground process group.
   Retries interrupted calls. Returns success and the final errno.

   TCSETPGRP sends SIGTTOU when the caller is currently in a background
   process group. Library mode blocks it only for this operation."
  (terminal--call-with-sigttou-safe
   process-group
   (lambda ()
     (loop
       (when (zerop (external-call "tcsetpgrp"
                                   :int 0
                                   :int process-group
                                   :int))
         (return (values t 0)))
       (let ((code (ccl::get-errno)))
         (unless (= code +terminal-eintr+)
           (return (values nil code))))))))

(defun terminal-current-foreground ()
  "Return the terminal's foreground process group and final errno."
  (loop
    (let ((process-group (external-call "tcgetpgrp" :int 0 :int)))
      (unless (minusp process-group)
        (return (values process-group 0))))
    (let ((code (ccl::get-errno)))
      (unless (= code +terminal-eintr+)
        (return (values nil code))))))

(defun terminal-foreground-checked (process-group
                                    &key (operation "foreground"))
  "Make PROCESS-GROUP foreground or signal TERMINAL-CONTROL-ERROR."
  (multiple-value-bind (success code)
      (terminal-foreground process-group)
    (unless success
      (error 'terminal-control-error
             :operation operation
             :process-group process-group
             :code code)))
  (values))

(defun process-group-continue (process-group)
  "Send SIGCONT to PROCESS-GROUP, unsticking a child that touched the
   terminal in the window before it became the foreground group."
  (external-call "kill" :int (- process-group) :int +sigcont+ :int)
  (values))

(defun process-group-stop (process-group)
  "Send SIGTSTP to PROCESS-GROUP, carrying a stop across every process
   group of a multi-stage job."
  (external-call "kill" :int (- process-group) :int +sigtstp+ :int)
  (values))

;;; Presentation wrappers

(defun terminal-colorize (text color &key bold)
  "Colorize TEXT only for terminal presentation output."
  (if (terminal-output-tty-p)
      (ansi-colorize text color :bold bold)
      text))

(defun terminal-fresh-line ()
  "Ensure the next output starts at column 0 of a fresh line. On a
   terminal this uses the fish trick: print a reverse video return
   marker followed by a line of spaces and a carriage return. When the
   previous output ended mid-line the spaces wrap, leaving the marker
   visible; at column 0 the next write simply overwrites it. Off
   terminals this falls back to FRESH-LINE."
  (if (terminal-output-tty-p)
      (multiple-value-bind (rows columns)
          (terminal-size)
        (declare (ignore rows))
        (write-string (ansi-reverse-video "⏎"))
        (loop repeat (max 0 (- columns 2))
              do (write-char #\space))
        (write-char #\return)
        (write-string (ansi-clear-line-right))
        (force-output))
      (fresh-line))
  (values))
