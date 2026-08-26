;;;; -- SBCL environment backend --

(in-package #:cclsh)

(define-condition environment-error (error)
  ((operation :initarg :operation :reader environment-error-operation)
   (name :initarg :name :reader environment-error-name)
   (code :initarg :code :reader environment-error-code))
  (:report
   (lambda (condition stream)
     (format stream "cannot ~a environment variable ~a~@[: errno ~d~]"
             (environment-error-operation condition)
             (environment-error-name condition)
             (environment-error-code condition)))))

(defvar *environment-lock* (ccl:make-lock "cclsh environment")
  "Serializes environment reads and mutations.")

(defparameter +package-environment-variable+ "CCLSH_PACKAGE"
  "Environment variable containing the current Lisp package name.")

(defun environment-name (designator)
  "Normalize an environment variable designator to its name string."
  (etypecase designator
    (symbol (symbol-name designator))
    (string designator)))

(defun environment--set-string (name text)
  "Set NAME to TEXT while the caller owns *ENVIRONMENT-LOCK*."
  (handler-case
      (progn
        (sb-posix:setenv name text 1)
        text)
    (sb-posix:syscall-error (condition)
      (error 'environment-error
             :operation "set"
             :name name
             :code (sb-posix:syscall-errno condition)))))

(defun environment--replace-exact (bindings)
  "Replace the process environment with string BINDINGS.
SBCL's POSIX interface has no CLEARENV binding, so remove the existing names
before applying the requested snapshot while holding the environment lock."
  (ccl:with-lock-grabbed (*environment-lock*)
    (dolist (entry (sb-ext:posix-environ))
      (let ((separator (position #\= entry)))
        (when separator
          (ignore-errors (sb-posix:unsetenv (subseq entry 0 separator))))))
    (dolist (binding bindings)
      (environment--set-string (car binding) (cdr binding))))
  (values))

(defun environment--package-name ()
  "Return the current package name, or the empty string."
  (or (package-name *package*) ""))

(defun environment--package-sync ()
  "Update CCLSH_PACKAGE while the caller owns *ENVIRONMENT-LOCK*."
  (environment--set-string +package-environment-variable+
                           (environment--package-name)))

(defun environment-package-sync ()
  "Set CCLSH_PACKAGE to the canonical name of the current Lisp package."
  (ccl:with-lock-grabbed (*environment-lock*)
    (environment--package-sync)))

(defun environment-call-with-package (function)
  "Call FUNCTION with CCLSH_PACKAGE synchronized for child processes."
  (ccl:with-lock-grabbed (*environment-lock*)
    (environment--package-sync)
    (funcall function)))

(defun getenv (name)
  "Return NAME's value, or NIL when it is absent."
  (ccl:with-lock-grabbed (*environment-lock*)
    (sb-posix:getenv (environment-name name))))

(defun setenv (name value)
  "Set NAME to VALUE and return VALUE."
  (let ((name (environment-name name))
        (text (if (stringp value) value (princ-to-string value))))
    (ccl:with-lock-grabbed (*environment-lock*)
      (environment--set-string name text)))
  value)

(defun unsetenv (name)
  "Remove NAME from the process environment."
  (let ((name (environment-name name)))
    (ccl:with-lock-grabbed (*environment-lock*)
      (handler-case
          (sb-posix:unsetenv name)
        (sb-posix:syscall-error (condition)
          (error 'environment-error
                 :operation "unset"
                 :name name
                 :code (sb-posix:syscall-errno condition))))))
  (values))

(defun env (name)
  "Setf-able accessor for environment variable NAME."
  (getenv name))

(defun (setf env) (value name)
  "Set NAME to VALUE in the process environment."
  (setenv name value))

(defun environment-variables ()
  "Return a sorted live NAME=value environment snapshot."
  (ccl:with-lock-grabbed (*environment-lock*)
    (environment--package-sync)
    (sort (copy-list (sb-ext:posix-environ)) #'string<)))
