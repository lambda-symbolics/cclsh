;;;; -- Definition editing --
;;;
;;; Open a recorded function definition in the user's editor, render the saved
;;; before/after text with Colordiff, and install it in the running image.

(in-package #:cclsh)

(define-condition definition-edit-error (shell-error)
  ((message :initarg :message :reader definition-edit-error-message))
  (:report (lambda (condition stream)
             (format stream "edit: ~a"
                     (definition-edit-error-message condition)))))

(defvar *definition-editor-runner* nil
  "Optional function called with editor argument words and the temporary path.
NIL uses RUN. This hook exists so the editor workflow can be tested without a
terminal editor.")

(defun definition-edit--fail (control &rest arguments)
  "Signal a DEFINITION-EDIT-ERROR with a formatted message."
  (error 'definition-edit-error
         :message (apply #'format nil control arguments)))

(defun definition-edit--read-designator (text)
  "Read one Lisp function designator from TEXT in the current package."
  (handler-case
      (multiple-value-bind (value position)
          (read-from-string text nil nil)
        (unless value
          (definition-edit--fail "empty definition designator"))
        (unless (every (lambda (character)
                         (find character '(#\Space #\Tab #\Newline #\Return)))
                       (subseq text position))
          (definition-edit--fail "invalid definition designator: ~a" text))
        value)
    (reader-error ()
      (definition-edit--fail "invalid definition designator: ~a" text))))

(defun definition-edit--function (designator)
  "Resolve DESIGNATOR to the function object whose source should be edited."
  (let ((name (if (stringp designator)
                  (definition-edit--read-designator designator)
                  designator)))
    (cond ((functionp name)
           name)
          ((or (symbolp name)
               (and (consp name)
                    (eq (first name) 'setf)
                    (null (cddr name))
                    (symbolp (second name))))
           (unless (fboundp name)
             (definition-edit--fail "~s is not fbound" name))
           (fdefinition name))
          (t
           (definition-edit--fail
            "~s is not a function, symbol, or SETF function name" name)))))

(defun definition-edit--source (designator)
  "Return source text, pathname, starting line, function and function name."
  (let* ((function (definition-edit--function designator))
         (name (ccl:function-name function)))
    (unless name
      (definition-edit--fail "the function has no name to redefine"))
    (dolist (target (list function name))
      (dolist (entry (ccl:find-definition-sources target 'function))
        (dolist (note (rest entry))
          (when (ccl:source-note-p note)
            (ccl:ensure-source-note-text note)
            (let ((text (ccl:source-note-text note)))
              (when text
                (let* ((pathname (ccl:source-note-filename note))
                       (line
                         (and pathname
                              (definition-edit--source-line
                               pathname
                               (ccl:source-note-start-pos note)))))
                  (return-from definition-edit--source
                    (values text pathname line function name)))))))))
    (definition-edit--fail "no recorded source is available for ~s" designator)))

(defun definition-edit--source-line (pathname byte-position)
  "Return the one-based line at BYTE-POSITION in PATHNAME, or NIL."
  (when (and pathname byte-position)
    (handler-case
        (with-open-file (stream pathname
                                :direction ':input
                                :element-type '(unsigned-byte 8))
          (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
                (remaining byte-position)
                (line 1))
            (loop while (plusp remaining)
                  for count = (read-sequence buffer stream
                                             :end (min remaining
                                                       (length buffer)))
                  while (plusp count)
                  do (incf line (count #x0a buffer :end count))
                     (decf remaining count))
            line))
      (file-error () nil))))

(defun definition-edit--editor-words ()
  "Return editor argv from VISUAL, EDITOR, or the portable VI fallback."
  (let* ((visual (getenv "VISUAL"))
         (editor (getenv "EDITOR"))
         (specification
           (cond ((and visual (plusp (length visual))) visual)
                 ((and editor (plusp (length editor))) editor)
                 (t "vi"))))
    (handler-case
        (let ((words (command-line-words specification)))
          (unless words
            (definition-edit--fail "editor command is empty"))
          words)
      (shell-error (condition)
        (error condition))
      (error (condition)
        (definition-edit--fail "invalid editor command ~s: ~a"
                               specification condition)))))

(defun definition-edit--run-editor (words pathname)
  "Run editor WORDS on PATHNAME and return its status."
  (if *definition-editor-runner*
      (funcall *definition-editor-runner* words pathname)
      (apply #'run (first words) (append (rest words)
                                         (list (namestring pathname))))))

(defun definition-edit--read-file (pathname)
  "Read PATHNAME completely as UTF-8 text."
  (with-open-file (stream pathname
                          :direction ':input
                          :external-format ':utf-8)
    (let ((text (make-string (file-length stream))))
      (let ((count (read-sequence text stream)))
        (if (= count (length text))
            text
            (subseq text 0 count))))))

(defun definition-edit--name-symbol (name)
  "Return the defining symbol contained in function NAME."
  (cond ((symbolp name) name)
        ((and (consp name) (eq (first name) 'setf) (symbolp (second name)))
         (second name))
        (t
         (find-if #'symbolp name))))

(defun definition-edit--read-forms (text package)
  "Read every top-level form from TEXT in PACKAGE before evaluating any."
  (let ((*package* package)
        (eof (gensym "EOF"))
        (forms nil))
    (with-input-from-string (stream text)
      (loop for form = (read stream nil eof)
            until (eq form eof)
            do (push form forms)))
    (unless forms
      (definition-edit--fail "the edited definition is empty"))
    (nreverse forms)))

(defun definition-edit--record-source (name text)
  "Record TEXT as the current interactive source for function NAME."
  (let* ((octets (ccl:encode-string-to-octets
                  text :external-format ':utf-8))
         (note (ccl::make-source-note
                :filename nil
                :start-pos 0
                :end-pos (length octets)
                :source octets)))
    (ccl:record-source-file name 'function note)))

(defun definition-edit--evaluate (text name)
  "Evaluate edited TEXT in NAME's package and retain it as current source."
  (let* ((symbol (definition-edit--name-symbol name))
         (package (or (and symbol (symbol-package symbol)) *package*))
         (forms (definition-edit--read-forms text package))
         (*package* package))
    (dolist (form forms)
      (eval form))
    (definition-edit--record-source name text)
    (values)))

(defun definition-edit--render-diff (before after pathname start-line)
  "Write a Lisp-highlighted Colordiff rendering for BEFORE and AFTER."
  (colordiff:write-ansi
   (colordiff:render-diff
    :removed-content before
    :added-content after
    :removed-start-line start-line
    :added-start-line start-line
    :source-path (or pathname "definition.lisp"))
   :stream *standard-output*)
  (force-output)
  0)

(defcommand edit (definition)
  "Edit a recorded function definition in VISUAL or EDITOR, install it in the
running image, and show a syntax-highlighted diff. DEFINITION may be a function,
symbol, or SETF name."
  (multiple-value-bind (before source-path start-line function name)
      (definition-edit--source definition)
    (declare (ignore function))
    (let ((editor-words (definition-edit--editor-words)))
      (uiop:with-temporary-file
          (:stream stream
           :pathname temporary-path
           :prefix "cclsh-edit-"
           :suffix ".lisp"
           :external-format ':utf-8)
        (write-string before stream)
        :close-stream
        (let ((status (definition-edit--run-editor editor-words temporary-path)))
          (unless (zerop status)
            (return-from edit status)))
        (let ((after (definition-edit--read-file temporary-path)))
          (definition-edit--read-forms
           after
           (or (symbol-package (definition-edit--name-symbol name)) *package*))
          (definition-edit--render-diff before after source-path start-line)
          (definition-edit--evaluate after name)
          (format t "Updated ~s in the running image.~%" name)
          (force-output)
          0)))))
