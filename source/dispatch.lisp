;;;; -- Dispatch --
;;;
;;; Turns one complete input line into action: Lisp evaluation for
;;; lines starting with an open paren, command execution otherwise.
;;; Errors never reach the CCL debugger; they print in red and the
;;; shell keeps going.

(in-package #:cclsh)

(defun dispatch-report-error (condition)
  "Print CONDITION in red on standard error."
  (format *error-output* "~a~%"
          (ansi-colorize (format nil "cclsh: ~a" condition) ':red))
  (force-output *error-output*)
  (values))

(defun dispatch--undefined-function-hint (form)
  "When FORM is a call whose head symbol names no function, macro or
   special operator, report it with close completions and return true.
   The form is then not evaluated: argument evaluation would surface a
   less useful error first, and would run side effects for a call that
   cannot succeed."
  (let ((head (and (consp form) (first form))))
    (when (and head
               (symbolp head)
               (not (special-operator-p head))
               (not (macro-function head))
               (not (fboundp head)))
      (let ((candidates
              (remove-if-not
               (lambda (name)
                 (multiple-value-bind (symbol found)
                     (find-symbol (string-upcase name) *package*)
                   (and found (fboundp symbol))))
               (completion--symbols (string-downcase (symbol-name head))))))
        (format *error-output* "~a~%"
                (ansi-colorize
                 (format nil "cclsh: undefined function ~(~a~)~
                              ~@[, did you mean ~{~a~^, ~}?~]"
                         head
                         (and candidates
                              (subseq candidates
                                      0 (min 3 (length candidates)))))
                 ':red))
        (force-output *error-output*))
      t)))

(defun dispatch-lisp (line)
  "Evaluate LINE as Lisp forms, print the values REPL style and update
   the * ** *** variables. Returns a shell helper's recorded status,
   zero after ordinary successful Lisp, or one after an error."
  (handler-case
      (let ((position 0)
            (eof      (list nil))
            (*lisp-dispatch-status-cell* (list nil)))
        (loop
          (multiple-value-bind (form next)
              (read-from-string line nil eof :start position)
            (when (eq form eof)
              (return (or (first *lisp-dispatch-status-cell*) 0)))
            (setf position next)
            (when (dispatch--undefined-function-hint form)
              (return 1))
            (let ((values (multiple-value-list (eval form))))
              (setf *** **
                    **  *
                    *   (first values))
              (terminal-fresh-line)
              (dolist (value values)
                (format t "~s~%" value))))))
    (serious-condition (condition)
      (dispatch-report-error condition)
      1)))

(defun dispatch--lone-value-status (line)
  "REPL fallback once command resolution has failed: a lone word that
   names a bound variable or keyword, or is a number literal,
   evaluates and prints as Lisp. Works on the raw LINE so glob
   characters in names like *earmuffed* variables cannot mangle it.
   Returns the exit status, or NIL when the fallback does not apply."
  (let ((trimmed (string-trim *whitespace-characters* line)))
    (when (and (plusp (length trimmed))
               (notany #'whitespace-char-p trimmed)
               (word-evaluates-alone-p trimmed))
      (dispatch-lisp trimmed))))

(defun dispatch--report-red (text)
  "Print TEXT in red on standard error."
  (format *error-output* "~a~%" (ansi-colorize text ':red))
  (force-output *error-output*)
  (values))

(defun dispatch--implicit-directory-status (words background-p)
  "Change to a sole directory in WORDS, or return NIL when not applicable.
Reject a background change like any other builtin command."
  (let ((directory (implicit-directory-path words)))
    (when directory
      (if background-p
          (progn
            (dispatch--report-red
             "cclsh: cannot background an implicit directory change")
            1)
          (cd directory)))))

(defun dispatch-command (line)
  "Execute LINE as a shell command line. Returns an exit status. An
   unescaped trailing & launches the command as a background job."
  (handler-case
      (multiple-value-bind (line background)
          (command-line-background-split line)
        (let ((words (command-line-words line)))
          (cond ((and (null words) background)
                 (dispatch--report-red "cclsh: & needs a command")
                 2)
                ((null words)
                 *last-status*)
                (t
                 (multiple-value-bind (kind target)
                     (command-resolve-fresh (first words))
                   (let ((*job-command-label*
                           (string-trim *whitespace-characters* line)))
                     (ecase kind
                       (:builtin
                        (cond (background
                               (dispatch--report-red
                                (format nil "cclsh: cannot background the ~
                                             builtin ~a"
                                        (first words)))
                               1)
                              (t
                               (command-execute-builtin target
                                                        (rest words)))))
                       (:external
                        (if background
                            (command-execute-background target (rest words))
                            (command-execute-external target (rest words))))
                       (:unknown
                        (or (dispatch--implicit-directory-status
                             words background)
                            (and (not background)
                                 (dispatch--lone-value-status line))
                            (error 'command-not-found-error
                                   :name (first words)))))))))))
    (shell-error (condition)
      (dispatch-report-error condition)
      127)
    (serious-condition (condition)
      (dispatch-report-error condition)
      1)))

(defun dispatch-line-executable-p (line)
  "True when LINE denotes a command or Lisp form rather than ignored input."
  (let ((trimmed (string-trim *whitespace-characters* line)))
    (and (plusp (length trimmed))
         (not (line-comment-p trimmed))
         (not (and (>= (length trimmed) 2)
                   (string= "#!" trimmed :end2 2))))))
(defun dispatch-line (line)
  "Execute LINE and return its exit status, recording *LAST-STATUS*.
   Blank and top-level comment lines leave the stopped jobs exit warning
   armed; any other non-blank line rearms it."
  (let* ((trimmed (string-trim *whitespace-characters* line))
         (comment-p (line-comment-p trimmed))
         (*jobs-exit-confirmed*
           (if (or (zerop (length trimmed)) comment-p)
               *jobs-exit-warned*
               (shiftf *jobs-exit-warned* nil))))
    (setf *last-status*
          (cond ((or (zerop (length trimmed)) comment-p)
                 *last-status*)
                ((and (>= (length trimmed) 2)
                      (string= "#!" trimmed :end2 2))
                 *last-status*)
                ((line-lisp-p trimmed)
                 (dispatch-lisp line))
                (t
                 (dispatch-command line))))))
