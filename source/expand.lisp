;;;; -- Word expansion --
;;;
;;; Tilde, environment variable and glob expansion for command mode
;;; words, plus quote removal. Globbing walks directories manually so
;;; patterns behave like a traditional shell: * and ? per path segment,
;;; hidden entries only match patterns that spell out the leading dot,
;;; and patterns without matches pass through literally.

(in-package #:cclsh)

(defun home-directory ()
  "Return the home directory as a string without a trailing slash."
  (let ((home (or (getenv "HOME")
                  (namestring (user-homedir-pathname)))))
    (if (string= home "/")
        home
        (string-right-trim "/" home))))

(defun tilde-expand (word)
  "Expand a leading ~ in WORD to the home directory."
  (cond ((string= word "~")
         (home-directory))
        ((and (> (length word) 1)
              (char= (char word 0) #\~)
              (char= (char word 1) #\/))
         (concatenate 'string (home-directory) (subseq word 1)))
        (t
         word)))


;;; Environment variables

(defun variable-name-char-p (char)
  "True when CHAR may appear in an environment variable name."
  (or (alphanumericp char) (char= char #\_)))

(defun variable-reference (text index)
  "Parse a $NAME or ${NAME} reference in TEXT at INDEX (at the $).
   Returns (values expansion next-index), or (values nil nil) when the
   $ does not start a reference. Unset variables expand to an empty
   string."
  (let ((length (length text)))
    (cond ((>= (1+ index) length)
           (values nil nil))
          ((char= (char text (1+ index)) #\{)
           (let ((close (position #\} text :start (+ index 2))))
             (if close
                 (values (or (getenv (subseq text (+ index 2) close)) "")
                         (1+ close))
                 (values nil nil))))
          ((and (variable-name-char-p (char text (1+ index)))
                (not (digit-char-p (char text (1+ index)))))
           (let ((end (or (position-if-not #'variable-name-char-p text
                                           :start (1+ index))
                          length)))
             (values (or (getenv (subseq text (1+ index) end)) "") end)))
          (t
           (values nil nil)))))

(defun variable-expand (text &key keep-escapes)
  "Expand $NAME and ${NAME} references in TEXT. A backslash escapes the
   following character; with KEEP-ESCAPES the backslash itself is kept
   in the output so a later pass can still honor it."
  (with-output-to-string (expanded)
    (let ((index  0)
          (length (length text)))
      (loop while (< index length)
            do (let ((char (char text index)))
                 (cond ((and (char= char #\\) (< (1+ index) length))
                        (when keep-escapes
                          (write-char #\\ expanded))
                        (write-char (char text (1+ index)) expanded)
                        (incf index 2))
                       ((char= char #\$)
                        (multiple-value-bind (expansion next)
                            (variable-reference text index)
                          (cond (expansion
                                 (write-string expansion expanded)
                                 (setf index next))
                                (t
                                 (write-char char expanded)
                                 (incf index)))))
                       (t
                        (write-char char expanded)
                        (incf index))))))))

(defun escape-remove (text)
  "Remove backslash escapes from TEXT, keeping the escaped characters."
  (with-output-to-string (clean)
    (let ((index  0)
          (length (length text)))
      (loop while (< index length)
            do (let ((char (char text index)))
                 (cond ((and (char= char #\\) (< (1+ index) length))
                        (write-char (char text (1+ index)) clean)
                        (incf index 2))
                       (t
                        (write-char char clean)
                        (incf index))))))))


;;; Globbing

(defun glob-pattern-p (text)
  "True when TEXT contains an unescaped * or ? wildcard."
  (loop with index = 0
        while (< index (length text))
        do (let ((char (char text index)))
             (cond ((char= char #\\)
                    (incf index 2))
                   ((member char '(#\* #\?))
                    (return t))
                   (t
                    (incf index))))
        finally (return nil)))

(defun glob-match-p (pattern name)
  "Match NAME against PATTERN with * and ? wildcards. A backslash in
   PATTERN escapes the next character. Hidden names only match when the
   pattern itself starts with a literal dot."
  (if (and (plusp (length name))
           (char= (char name 0) #\.)
           (not (and (plusp (length pattern))
                     (char= (char pattern 0) #\.))))
      nil
      (glob--match-index pattern 0 name 0)))

(defun glob--match-index (pattern pattern-index name name-index)
  "Recursive matcher behind GLOB-MATCH-P."
  (let ((pattern-length (length pattern))
        (name-length    (length name)))
    (cond ((>= pattern-index pattern-length)
           (>= name-index name-length))
          ((char= (char pattern pattern-index) #\*)
           (loop for skip from name-index to name-length
                 when (glob--match-index pattern (1+ pattern-index) name skip)
                   return t))
          ((>= name-index name-length)
           nil)
          ((char= (char pattern pattern-index) #\?)
           (glob--match-index pattern (1+ pattern-index) name (1+ name-index)))
          ((char= (char pattern pattern-index) #\\)
           (and (< (1+ pattern-index) pattern-length)
                (char= (char pattern (1+ pattern-index)) (char name name-index))
                (glob--match-index pattern (+ pattern-index 2) name (1+ name-index))))
          (t
           (and (char= (char pattern pattern-index) (char name name-index))
                (glob--match-index pattern (1+ pattern-index) name (1+ name-index)))))))

(defun path-exists-p (path)
  "True when PATH names an existing file or directory."
  (not (null (ignore-errors (probe-file path)))))

(defun directory-exists-p (path)
  "True when PATH names an existing directory."
  (let ((found (ignore-errors (probe-file path))))
    (and found (pathname-directory-form-p found) t)))

(defun implicit-directory-path-p (path)
  "True when PATH has the explicit shape required for an implicit cd.

Like fish, cclsh accepts .., absolute paths, paths beginning with ./ or ../,
and paths ending in a slash. A bare directory name remains a command name
unless completion adds its trailing slash."
  (let ((length (length path)))
    (and (plusp length)
         (or (string= path "..")
             (char= (char path 0) #\/)
             (and (>= length 2)
                  (char= (char path 0) #\.)
                  (char= (char path 1) #\/))
             (and (>= length 3)
                  (char= (char path 0) #\.)
                  (char= (char path 1) #\.)
                  (char= (char path 2) #\/))
             (char= (char path (1- length)) #\/)))))

(defun implicit-directory-path (words)
  "Return WORDS' sole explicit directory path, or NIL.

Command expansion may turn one source word into several glob matches, so an
implicit directory change is safe only when the final command has one word."
  (when (and (consp words)
             (null (rest words))
             (implicit-directory-path-p (first words))
             (directory-exists-p (first words)))
    (first words)))

(defun directory-entry-names (directory)
  "List the entry names in DIRECTORY (a string; empty means the current
   directory). Returns (values file-names directory-names)."
  (let* ((base    (cond ((string= directory "")
                         "./")
                        ((char= (char directory (1- (length directory))) #\/)
                         directory)
                        (t
                         (concatenate 'string directory "/"))))
         (wild    (merge-pathnames (make-pathname :name ':wild :type ':wild) base))
         (entries (ignore-errors
                    #+ccl
                    (directory wild :directories t :files t :follow-links nil)
                    #+sbcl
                    (directory wild))))
    (let ((files          nil)
          (subdirectories nil))
      (dolist (entry entries)
        ;; Native namestrings avoid CCL's backslash quoting of wild
        ;; characters, so a file literally named st*ar.txt lists as
        ;; itself rather than as st\*ar.txt.
        (let* ((native  (ignore-errors
                          (ccl:native-translated-namestring entry)))
               (trimmed (and native (string-right-trim "/" native)))
               (name    (and trimmed
                             (subseq trimmed
                                     (1+ (or (position #\/ trimmed
                                                       :from-end t)
                                             -1))))))
          (when (and name (plusp (length name)))
            (if (pathname-directory-form-p entry)
                (push name subdirectories)
                (push name files)))))
      (values files subdirectories))))

(defun glob--split-segments (pattern)
  "Split PATTERN on slashes, dropping empty segments."
  (loop with segments = nil
        with start = 0
        for slash = (position #\/ pattern :start start)
        do (let ((piece (subseq pattern start slash)))
             (when (plusp (length piece))
               (push piece segments))
             (if slash
                 (setf start (1+ slash))
                 (return (nreverse segments))))))

(defun glob--join (prefix name)
  "Join a path PREFIX and an entry NAME with a slash as needed."
  (cond ((string= prefix "")
         name)
        ((string= prefix "/")
         (concatenate 'string "/" name))
        (t
         (concatenate 'string prefix "/" name))))

(defun glob--expand-segment (prefixes segment last-segment-p)
  "Expand one path SEGMENT against each prefix in PREFIXES."
  (loop for prefix in prefixes
        append
        (if (glob-pattern-p segment)
            (multiple-value-bind (files subdirectories)
                (directory-entry-names prefix)
              (loop for name in (if last-segment-p
                                    (append files subdirectories)
                                    subdirectories)
                    when (glob-match-p segment name)
                      collect (glob--join prefix name)))
            (let ((candidate (glob--join prefix (escape-remove segment))))
              (if (if last-segment-p
                      (path-exists-p candidate)
                      (directory-exists-p candidate))
                  (list candidate)
                  nil)))))

(defun glob-expand (pattern)
  "Expand PATTERN against the filesystem. Returns a sorted list of
   matching path strings, or NIL when nothing matches."
  (let* ((absolute (and (plusp (length pattern))
                        (char= (char pattern 0) #\/)))
         (segments (glob--split-segments pattern))
         (prefixes (list (if absolute "/" ""))))
    (loop for (segment . remaining) on segments
          do (setf prefixes (glob--expand-segment prefixes segment (null remaining)))
          while prefixes)
    (sort (remove-duplicates prefixes :test #'string=) #'string<)))

(defun glob (&rest patterns)
  "Expand string PATTERNS into one flat argument list.

Expansion uses the same tilde, environment-variable, * and ? rules as a bare
command word. Matches are sorted within each pattern, pattern order is
preserved, and a pattern with no matches remains literal. A proper list used
as a CMD, PIPE, CAPTURE, SEQ, ALL or ANY stage argument is spliced into that
stage's argument vector, so (glob \"*.lisp\") composes directly with them."
  (loop for pattern in patterns
        do (check-type pattern string)
        append (word-expand pattern)))


;;; Lisp substitution

(defun lisp-substitution--inner (text)
  "Strip the delimiters from a :lisp token TEXT: a leading ( or $( and
   the closing paren when present."
  (let* ((start (if (char= (char text 0) #\$) 2 1))
         (end   (if (and (> (length text) start)
                         (char= (char text (1- (length text))) #\)))
                    (1- (length text))
                    (length text))))
    (subseq text start end)))

(defun lisp-substitution--forms (inner)
  "Read all forms from the substitution body INNER."
  (let ((forms    nil)
        (position 0)
        (eof      (list nil)))
    (loop
      (multiple-value-bind (form next)
          (read-from-string inner nil eof :start position)
        (when (eq form eof)
          (return (nreverse forms)))
        (push form forms)
        (setf position next)))))

(defun lisp-substitution-value (text)
  "Evaluate the :lisp substitution TEXT and return its value. The
   outer parens are shell delimiters: a single form inside evaluates
   as written, so (*balls*) is the variable *balls*, while several
   forms become one function call, so (+ 1 2) applies +."
  (let ((forms (lisp-substitution--forms (lisp-substitution--inner text))))
    (cond ((null forms)
           nil)
          ((null (rest forms))
           (eval (first forms)))
          (t
           (eval forms)))))

(defun lisp-substitution-words (text)
  "Expand a standalone :lisp substitution into argument words: NIL
   vanishes, a proper list splices into several words, anything else
   is one PRINC-TO-STRINGed word."
  (let ((value (lisp-substitution-value text)))
    (cond ((null value)
           nil)
          ((and (listp value)
                (ignore-errors (list-length value)))
           (mapcar #'princ-to-string value))
          (t
           (list (princ-to-string value))))))


;;; Word level expansion

(defun word-expand (text)
  "Expand a bare word: tilde, variables, then globbing. Returns a list
   of resulting words since a glob may produce several."
  (let* ((tilded   (tilde-expand text))
         (expanded (variable-expand tilded :keep-escapes t)))
    (if (glob-pattern-p expanded)
        (or (glob-expand expanded)
            (list (escape-remove expanded)))
        (list (escape-remove expanded)))))

(defun quote-strip (text)
  "Remove the surrounding quotes from a quote token's TEXT."
  (let ((length (length text)))
    (if (and (>= length 2)
             (char= (char text (1- length)) (char text 0)))
        (subseq text 1 (1- length))
        (subseq text 1))))

(defun double-quote-expand (text)
  "Expand the inside of a double-quoted TEXT (quotes already removed):
   variables expand, a backslash escapes $, \", backslash and backquote
   and stays literal before anything else."
  (with-output-to-string (expanded)
    (let ((index  0)
          (length (length text)))
      (loop while (< index length)
            do (let ((char (char text index)))
                 (cond ((and (char= char #\\)
                             (< (1+ index) length)
                             (find (char text (1+ index)) "$\"\\`"))
                        (write-char (char text (1+ index)) expanded)
                        (incf index 2))
                       ((char= char #\$)
                        (multiple-value-bind (expansion next)
                            (variable-reference text index)
                          (cond (expansion
                                 (write-string expansion expanded)
                                 (setf index next))
                                (t
                                 (write-char char expanded)
                                 (incf index)))))
                       (t
                        (write-char char expanded)
                        (incf index))))))))

(defun token-expand (source token)
  "Expand TOKEN into its literal argument text, without globbing.
   Inside a larger word a :lisp substitution concatenates: NIL becomes
   an empty string, other values PRINC-TO-STRING."
  (let ((text (token-text source token)))
    (ecase (token-type token)
      (:word         (escape-remove (variable-expand (tilde-expand text))))
      (:double-quote (double-quote-expand (quote-strip text)))
      (:single-quote (quote-strip text))
      (:lisp         (let ((value (lisp-substitution-value text)))
                       (if (null value)
                           ""
                           (princ-to-string value)))))))


;;; Argument groups

(defun token-groups (tokens)
  "Split TOKENS into argument groups separated by whitespace."
  (let ((groups  nil)
        (current nil))
    (dolist (token tokens)
      (if (eq (token-type token) ':space)
          (when current
            (push (nreverse current) groups)
            (setf current nil))
          (push token current)))
    (when current
      (push (nreverse current) groups))
    (nreverse groups)))

(defun group-expand (source group)
  "Expand one argument GROUP into a list of words. A single bare word
   may glob into several words, a standalone :lisp substitution may
   splice or vanish, and mixed groups concatenate into exactly one."
  (cond ((and (null (rest group))
              (eq (token-type (first group)) ':word))
         (word-expand (token-text source (first group))))
        ((and (null (rest group))
              (eq (token-type (first group)) ':lisp))
         (lisp-substitution-words (token-text source (first group))))
        (t
         (list (apply #'concatenate 'string
                      (mapcar (lambda (token)
                                (token-expand source token))
                              group))))))

(defun command-line-words (line)
  "Lex LINE and expand it into the final list of argument words."
  (loop for group in (token-groups (lex-command-line line))
        append (group-expand line group)))
