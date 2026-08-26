;;;; -- Prompt rendering --
;;;
;;; The built-in prompt may be replaced from startup.lisp. A custom renderer
;;; receives snapshots of shell state and returns a complete prompt string.

(in-package #:cclsh)

(defvar *prompt-function* nil
  "Custom prompt renderer function designator, or NIL for PROMPT-DEFAULT.
The renderer receives STATUS, DURATION-MILLISECONDS, COLUMNS and JOB-COUNT as
keyword arguments. It may return a string verbatim or NIL to use the default.")

(defun prompt--username ()
  "Return the operating-system name of the effective user."
  #+ccl
  (let ((uid (external-call "geteuid" :unsigned-int)))
    (or (ignore-errors
          ;; getpwuid returns a passwd structure whose first field is pw_name.
          ;; Copy the static result while this Lisp thread cannot be interrupted.
          (ccl:without-interrupts
            (let ((entry (external-call "getpwuid"
                                        :unsigned-int uid
                                        :address)))
              (unless (ccl:%null-ptr-p entry)
                (let ((name (ccl:%get-ptr entry)))
                  (unless (ccl:%null-ptr-p name)
                    (ccl::%get-utf-8-cstring name)))))))
        (format nil "~d" uid)))
  #+sbcl
  (or (getenv "USER")
      (getenv "LOGNAME")
      "unknown"))

(defun prompt--hostname ()
  "Return the operating-system hostname, with a stable fallback."
  (let ((hostname (ignore-errors (machine-instance))))
    (if (and (stringp hostname) (plusp (length hostname)))
        hostname
        "localhost")))

(defun prompt-default (&key
                         (status *last-status*)
                         duration-milliseconds
                         columns
                         job-count
                         &allow-other-keys)
  "Render the built-in colored prompt.
STATUS selects the status-sigil color. The remaining prompt context is
accepted so this function obeys the custom renderer protocol."
  (declare (ignore duration-milliseconds columns job-count))
  (let* ((identity  (format nil "~a@~a"
                            (prompt--username)
                            (prompt--hostname)))
         (package   (format nil "(~a)" (environment--package-name)))
         (directory (string-right-trim "/" (namestring (current-directory))))
         (home      (home-directory))
         (shortened (cond ((string= directory home)
                           "~")
                          ((and (> (length directory) (length home))
                                (string= home (subseq directory 0 (length home)))
                                (char= (char directory (length home)) #\/))
                           (concatenate 'string "~" (subseq directory (length home))))
                          ((string= directory "")
                           "/")
                          (t
                           directory))))
    (format nil "~a ~a ~a ~a "
            (ansi-colorize identity ':cyan :bold t)
            (ansi-colorize package ':magenta :bold t)
            (ansi-colorize shortened ':green :bold t)
            (ansi-colorize "$" (if (zerop status) ':white ':red)))))

(defun prompt--report-failure (control &rest arguments)
  "Report a custom prompt failure described by CONTROL and ARGUMENTS."
  (format *error-output* "~&~a~%"
          (terminal-colorize
           (apply #'format nil
                  (concatenate 'string "cclsh prompt: " control)
                  arguments)
           ':red))
  (force-output *error-output*)
  (values))

(defun prompt-render (status duration-milliseconds columns)
  "Render the next prompt from a stable snapshot of shell state.
CCLSH_PACKAGE is refreshed before custom code runs. Custom errors and NIL
results fall back to PROMPT-DEFAULT so prompt configuration cannot brick a
login shell."
  (let ((job-count (jobs-count)))
    (environment-package-sync)
    (flet ((render-default ()
             (prompt-default :status                status
                             :duration-milliseconds duration-milliseconds
                             :columns               columns
                             :job-count             job-count)))
      (if (null *prompt-function*)
          (render-default)
          (handler-case
              (let ((*last-status* status))
                (let ((result
                        (funcall *prompt-function*
                                 :status                status
                                 :duration-milliseconds duration-milliseconds
                                 :columns               columns
                                 :job-count             job-count)))
                  (cond ((stringp result)
                         result)
                        ((null result)
                         (render-default))
                        (t
                         (prompt--report-failure
                          "renderer returned ~s instead of a string or NIL"
                          result)
                         (render-default)))))
            (error (condition)
              (prompt--report-failure "renderer failed: ~a" condition)
              (render-default)))))))
