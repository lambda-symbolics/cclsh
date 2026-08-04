;;;; -- Environment variables --
;;;
;;; Lispy access to the process environment. Names are designators:
;;; (getenv 'editor), (getenv :editor) and (getenv "EDITOR") all name
;;; EDITOR since the reader upcases symbols, and ENV is a setf-able
;;; accessor. Lowercase variable names like http_proxy need strings.

(in-package #:cclsh)

(define-condition environment-error (error)
  ((operation
    :initarg :operation
    :reader environment-error-operation)
   (name
    :initarg :name
    :reader environment-error-name)
   (code
    :initarg :code
    :reader environment-error-code))
  (:documentation "Signaled when libc rejects an environment operation.")
  (:report
   (lambda (condition stream)
     (format stream "cannot ~a environment variable ~a~@[: ~a~]"
             (environment-error-operation condition)
             (environment-error-name condition)
             (let ((code (environment-error-code condition)))
               (when code
                 (let ((pointer
                         (external-call "strerror" :int code :address)))
                   (if (ccl:%null-ptr-p pointer)
                       (format nil "system error ~d" code)
                       (ccl::%get-utf-8-cstring pointer)))))))))

(defvar *environment-lock* (ccl:make-lock "cclsh environment")
  "Serializes libc environment reads and mutations.")

(defconstant +package-environment-variable+ "CCLSH_PACKAGE"
  "Environment variable containing the current Lisp package name.")

(defun environment-name (designator)
  "Normalize an environment variable DESIGNATOR to a name string.
   Symbols and keywords use their symbol name."
  (etypecase designator
    (symbol (symbol-name designator))
    (string designator)))

(defun environment--set-string (name text)
  "Set environment NAME to string TEXT while the caller holds the lock."
  (ccl::with-utf-8-cstr (encoded-name name)
    (ccl::with-utf-8-cstr (encoded-text text)
      (unless (zerop (external-call "setenv"
                                    :address encoded-name
                                    :address encoded-text
                                    :int 1
                                    :int))
        (error 'environment-error
               :operation "set"
               :name name
               :code (ccl::get-errno)))))
  text)

(defun environment--replace-exact (bindings)
  "Replace the libc environment with string BINDINGS.
BINDINGS contains (NAME . VALUE) pairs and the environment lock remains held
from CLEARENV through the last SETENV call, so CCLSH subprocess snapshots
cannot observe a partially replaced environment."
  (ccl:with-lock-grabbed (*environment-lock*)
    (unless (zerop (external-call "clearenv" :int))
      (error 'environment-error
             :operation "clear"
             :name "environment"
             :code (ccl::get-errno)))
    (dolist (binding bindings)
      (environment--set-string (car binding) (cdr binding))))
  (values))

(defun environment--package-name ()
  "Return the canonical name of the current package, or an empty string."
  (or (package-name *package*) ""))

(defun environment--package-sync ()
  "Update CCLSH_PACKAGE while the caller holds the environment lock."
  (environment--set-string +package-environment-variable+
                           (environment--package-name)))

(defun environment-package-sync ()
  "Set CCLSH_PACKAGE to the canonical name of the current Lisp package.
Return the name so a subprocess launcher can use the same stable snapshot."
  (ccl:with-lock-grabbed (*environment-lock*)
    (environment--package-sync)))

(defun environment-call-with-package (function)
  "Call FUNCTION while CCLSH_PACKAGE names the current Lisp package.
Hold the environment lock until FUNCTION returns so CCL subprocess launchers
inherit the same UTF-8 libc environment value without using CCL's lossy
per-process environment encoder."
  (ccl:with-lock-grabbed (*environment-lock*)
    (environment--package-sync)
    (funcall function)))

(defun getenv (name)
  "Value of the environment variable NAME (symbol or string), or NIL."
  (let ((name (environment-name name)))
    (ccl:with-lock-grabbed (*environment-lock*)
      (ccl::with-utf-8-cstr (encoded name)
        (let ((value (external-call "getenv" :address encoded :address)))
          (unless (ccl:%null-ptr-p value)
            (ccl::%get-utf-8-cstring value)))))))

(defun setenv (name value)
  "Set the environment variable NAME (symbol or string) to VALUE.
   Non-string values are stringified with PRINC-TO-STRING. Returns
   VALUE."
  (let ((name (environment-name name))
        (text (if (stringp value)
                  value
                  (princ-to-string value))))
    (ccl:with-lock-grabbed (*environment-lock*)
      (environment--set-string name text)))
  value)

(defun unsetenv (name)
  "Remove the environment variable NAME and return no values."
  (let ((name (environment-name name)))
    (ccl:with-lock-grabbed (*environment-lock*)
      (ccl::with-utf-8-cstr (encoded name)
        (unless (zerop (external-call "unsetenv" :address encoded :int))
          (error 'environment-error
                 :operation "unset"
                 :name name
                 :code (ccl::get-errno))))))
  (values))

(defun env (name)
  "Setf-able accessor for the environment variable NAME."
  (getenv name))

(defun (setf env) (value name)
  "Set the environment variable NAME to VALUE."
  (setenv name value))

(defun environment-variables ()
  "Return the current environment as a sorted list of NAME=value
   strings, read live from libc's environ. CCLSH_PACKAGE is refreshed
   in the same critical section so child-process snapshots stay coherent."
  (ccl:with-lock-grabbed (*environment-lock*)
    (environment--package-sync)
    (let ((environ
            (ccl:%get-ptr (ccl:foreign-symbol-address "environ")))
          (entries nil))
      (loop for index from 0
            for entry = (ccl:%get-ptr environ (* index 8))
            until (ccl:%null-ptr-p entry)
            do (push (ccl::%get-utf-8-cstring entry) entries))
      (sort entries #'string<))))
