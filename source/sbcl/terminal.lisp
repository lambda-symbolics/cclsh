;;;; -- SBCL terminal backend --
;;;
;;; SB-POSIX supplies the host-specific TERMIOS representation on both Linux
;;; and macOS.  The few process-group calls absent from SB-POSIX use CFFI's
;;; libc bindings, avoiding Linux-only structure layouts.

(in-package #:cclsh)

(cffi:defcfun ("isatty" terminal--isatty) :int (descriptor :int))
(cffi:defcfun ("tcsetpgrp" terminal--tcsetpgrp) :int
  (descriptor :int) (process-group :int))
(cffi:defcfun ("tcgetpgrp" terminal--tcgetpgrp) :int (descriptor :int))
(cffi:defcfun ("kill" terminal--kill) :int (process :int) (signal :int))
(cffi:defcfun ("signal" terminal--signal) :pointer
  (signal :int) (disposition :pointer))
(cffi:defcfun ("ioctl" terminal--ioctl) :int
  (descriptor :int) (request :unsigned-long) (argument :pointer))

(defconstant +winsize-ioctl+
  #+darwin #x40087468
  #-darwin #x5413
  "TIOCGWINSZ for the supported Unix hosts.")

(defconstant +sigcont+ sb-posix:sigcont)
(defconstant +sigtstp+ sb-posix:sigtstp)
(defconstant +sigttin+ sb-posix:sigttin)
(defconstant +sigttou+ sb-posix:sigttou)
(defconstant +terminal-eintr+ sb-posix:eintr)
(defconstant +terminal-esrch+ sb-posix:esrch)
(defconstant +terminal-eperm+ sb-posix:eperm)

(define-condition terminal-control-error (error)
  ((operation :initarg :operation :reader terminal-control-error-operation)
   (process-group :initarg :process-group
                  :reader terminal-control-error-process-group)
   (code :initarg :code :reader terminal-control-error-code))
  (:report
   (lambda (condition stream)
     (format stream "cannot ~a process group ~d: errno ~d"
             (terminal-control-error-operation condition)
             (terminal-control-error-process-group condition)
             (terminal-control-error-code condition)))))

(define-condition terminal-attributes-error (error)
  ((operation :initarg :operation :reader terminal-attributes-error-operation)
   (code :initarg :code :reader terminal-attributes-error-code))
  (:report
   (lambda (condition stream)
     (format stream "cannot ~a terminal attributes: errno ~d"
             (terminal-attributes-error-operation condition)
             (terminal-attributes-error-code condition)))))

(defvar *terminal-control-signals-active* nil)
(defvar *terminal-saved-termios* nil)
(defvar *terminal-shell-attributes* nil)

(defun terminal--errno ()
  "Return the current libc errno value."
  (cffi-sys::get-errno))

(defun terminal--call-with-sigttou-safe (process-group function)
  "Call FUNCTION while the shell's terminal signal policy is active."
  (declare (ignore process-group))
  (funcall function))

(defun terminal-tty-p ()
  "True when standard input is an interactive terminal."
  (= 1 (terminal--isatty 0)))

(defun terminal-output-tty-p ()
  "True when presentation output is an interactive terminal."
  (and *presentation-enabled* (= 1 (terminal--isatty 1))))

(defparameter *semantic-prompt-markers-enabled* t)
(defvar *semantic-command-marker-active* nil)

(defun terminal-semantic-marker (marker &optional (status 0))
  "Return Clinedi's OSC 133 sequence for MARKER and STATUS."
  (clinedi:semantic-prompt-marker-sequence marker status))

(defun terminal-write-semantic-marker
    (marker &optional (status 0) (stream *standard-output*)
             (enabled-p (and *semantic-prompt-markers-enabled*
                             (terminal-output-tty-p))))
  "Write one prompt lifecycle marker when terminal output permits it."
  (when enabled-p
    (write-string (terminal-semantic-marker marker status) stream)
    (force-output stream))
  (values))

(defun terminal-finish-semantic-command
    (&optional (status 0) (stream *standard-output*)
               (enabled-p (and *semantic-prompt-markers-enabled*
                               (terminal-output-tty-p))))
  "Emit the active command's completion marker once."
  (when *semantic-command-marker-active*
    (terminal-write-semantic-marker ':command-finished status stream enabled-p)
    (setf *semantic-command-marker-active* nil))
  (values))

(defun terminal--get-termios ()
  "Return a fresh SB-POSIX termios object, or NIL outside a terminal."
  (handler-case
      (sb-posix:tcgetattr 0)
    (sb-posix:syscall-error () nil)))

(defun terminal--copy-termios (attributes)
  "Return an independent copy of the SB-POSIX terminal ATTRIBUTES object."
  (make-instance 'sb-posix:termios
                 :iflag (sb-posix:termios-iflag attributes)
                 :oflag (sb-posix:termios-oflag attributes)
                 :cflag (sb-posix:termios-cflag attributes)
                 :lflag (sb-posix:termios-lflag attributes)
                 :cc (copy-seq (sb-posix:termios-cc attributes))))

(defun terminal--set-termios (attributes)
  "Apply ATTRIBUTES immediately. Return success and errno."
  (handler-case
      (progn
        (sb-posix:tcsetattr 0 sb-posix:tcsanow attributes)
        (values t 0))
    (sb-posix:syscall-error (condition)
      (values nil (sb-posix:syscall-errno condition)))))

(defun terminal-raw ()
  "Switch the terminal to unbuffered, non-echoing input mode."
  (let ((attributes (terminal--get-termios)))
    (when attributes
      (setf *terminal-saved-termios* (terminal--copy-termios attributes))
      (setf (sb-posix:termios-lflag attributes)
            (logandc2 (sb-posix:termios-lflag attributes)
                       (logior sb-posix:isig sb-posix:icanon sb-posix:echo))
            (aref (sb-posix:termios-cc attributes) sb-posix:vtime) 0
            (aref (sb-posix:termios-cc attributes) sb-posix:vmin) 1)
      (terminal--set-termios attributes))))

(defun terminal-restore ()
  "Restore the terminal state saved by TERMINAL-RAW."
  (when *terminal-saved-termios*
    (unwind-protect
        (multiple-value-bind (success code)
            (terminal--set-termios *terminal-saved-termios*)
          (unless success
            (error 'terminal-attributes-error :operation "restore" :code code)))
      (setf *terminal-saved-termios* nil)))
  (values))

(defun terminal-attributes ()
  "Return a snapshot of current terminal attributes, or NIL off-terminal."
  (let ((attributes (terminal--get-termios)))
    (and attributes (terminal--copy-termios attributes))))

(defun terminal-attributes-checked ()
  "Return a terminal snapshot or signal TERMINAL-ATTRIBUTES-ERROR."
  (or (terminal-attributes)
      (error 'terminal-attributes-error :operation "read" :code (terminal--errno))))

(defun terminal-attributes-apply (attributes)
  "Apply ATTRIBUTES as obtained from TERMINAL-ATTRIBUTES."
  (when attributes
    (multiple-value-bind (success code) (terminal--set-termios attributes)
      (unless success
        (error 'terminal-attributes-error :operation "apply" :code code))))
  (values))

(defun terminal-shell-attributes-save ()
  "Remember the terminal state at shell startup."
  (setf *terminal-shell-attributes* (terminal-attributes))
  (values))

(defun terminal-size ()
  "Return terminal rows and columns, defaulting to 24 by 80."
  (cffi:with-foreign-object (winsize :unsigned-short 4)
    (if (zerop (terminal--ioctl 0 +winsize-ioctl+ winsize))
        (let ((rows (cffi:mem-aref winsize :unsigned-short 0))
              (columns (cffi:mem-aref winsize :unsigned-short 1)))
          (values (if (plusp rows) rows 24)
                  (if (plusp columns) columns 80)))
        (values 24 80))))

(defun terminal--signal-disposition (signal disposition)
  "Set SIGNAL's disposition to the libc signal pointer DISPOSITION."
  (terminal--signal signal (cffi:make-pointer disposition))
  (values))

(defun terminal-signals-setup ()
  "Ignore terminal background-stop signals in the shell process."
  (terminal--signal-disposition +sigttou+ 1)
  (terminal--signal-disposition +sigttin+ 1)
  (values))

(defmacro with-terminal-control-signals (&body body)
  "Run BODY with CCLSH's terminal process-group signal policy."
  `(progn
     (terminal-signals-setup)
     (let ((*terminal-control-signals-active* t))
       ,@body)))

(defun terminal-own-process-group ()
  "Return the shell process group."
  (sb-posix:getpgrp))

(defun terminal-foreground (process-group)
  "Make PROCESS-GROUP own the terminal. Return success and errno."
  (terminal--call-with-sigttou-safe
   process-group
   (lambda ()
     (if (zerop (terminal--tcsetpgrp 0 process-group))
         (values t 0)
         (values nil (terminal--errno))))))

(defun terminal-current-foreground ()
  "Return the foreground process group and errno."
  (let ((process-group (terminal--tcgetpgrp 0)))
    (if (minusp process-group)
        (values nil (terminal--errno))
        (values process-group 0))))

(defun terminal-foreground-checked (process-group &key (operation "foreground"))
  "Make PROCESS-GROUP foreground or signal TERMINAL-CONTROL-ERROR."
  (multiple-value-bind (success code) (terminal-foreground process-group)
    (unless success
      (error 'terminal-control-error
             :operation operation :process-group process-group :code code)))
  (values))

(defun process-group-continue (process-group)
  "Send SIGCONT to every process in PROCESS-GROUP."
  (terminal--kill (- process-group) +sigcont+)
  (values))

(defun process-group-stop (process-group)
  "Send SIGTSTP to every process in PROCESS-GROUP."
  (terminal--kill (- process-group) +sigtstp+)
  (values))

(defun terminal-colorize (text color &key bold)
  "Colorize TEXT only when output is a terminal."
  (if (terminal-output-tty-p) (ansi-colorize text color :bold bold) text))

(defun terminal-fresh-line ()
  "Ensure the next output begins on a new terminal line."
  (if (terminal-output-tty-p)
      (multiple-value-bind (rows columns) (terminal-size)
        (declare (ignore rows))
        (write-string (ansi-reverse-video "⏎"))
        (loop repeat (max 0 (- columns 2)) do (write-char #\space))
        (write-char #\return)
        (write-string (ansi-clear-line-right))
        (force-output))
      (fresh-line))
  (values))
