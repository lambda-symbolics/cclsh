;;;; -- Completion --
;;;
;;; Tab completion for command names, file paths and Lisp symbols.
;;; COMPLETE-LINE inspects the buffer around the cursor and returns
;;; full replacement candidates; the line editor owns presentation.

(in-package #:cclsh)

(defun string-prefix-p (prefix string)
  "True when STRING starts with PREFIX."
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun path-command-names ()
  "Executable names found in PATH as a sorted prefix index.
   Cached until PATH changes or REHASH clears the cache."
  (let ((path (or (getenv "PATH") "")))
    (unless (and *path-command-names*
                 (equal path *path-command-names-source*))
      (let ((names (make-hash-table :test #'equal)))
        (dolist (directory (path-directories))
          (multiple-value-bind (files subdirectories)
              (directory-entry-names directory)
            (declare (ignore subdirectories))
            (dolist (file files)
              (setf (gethash file names) t))))
        (let ((unique (make-array (hash-table-count names)))
              (index 0))
          (maphash (lambda (name present)
                     (declare (ignore present))
                     (setf (aref unique index) name)
                     (incf index))
                   names)
          (setf *path-command-names*
                (structlisp:make-sorted-string-index
                 :initial-contents unique)))
        (setf *path-command-names-source* path)))
    *path-command-names*))

(defun shell-command-names ()
  "Names of the COMMAND instances currently visible, downcased."
  (let ((names nil))
    (do-symbols (symbol *package*)
      (when (and (boundp symbol)
                 (typep (symbol-value symbol) 'command))
        (pushnew (string-downcase (symbol-name symbol)) names
                 :test #'string=)))
    names))

(defparameter *completion-escaped-characters*
  (list #\space #\" #\' #\\ #\$ #\* #\? #\( #\) #\&)
  "Characters that would split or expand a completed word: whitespace,
   quotes, backslashes, variable references, glob wildcards, Lisp
   substitution parens and the background marker.")

(defun completion--terminal-control-p (char)
  "True when CHAR is a C0, DEL or C1 terminal control character."
  (let ((code (char-code char)))
    (or (< code 32)
        (= code 127)
        (<= 128 code 159))))

(defun completion--write-controlled-text (text stream)
  "Write a command expression for TEXT to STREAM without emitting any
   control characters. The expression expands back to the exact string."
  (format stream "$((map 'string #'code-char '~s))"
          (map 'list #'char-code text)))

(defun completion--escape (text &key (escape-leading-tilde t))
  "Backslash escape the special characters in TEXT so it survives
   being typed back into a command line. Terminal controls are written
   as Lisp substitutions so neither the edit buffer nor its rendering
   contains the raw control character. ESCAPE-LEADING-TILDE protects a
   literal command or filename; file paths under ~/ leave it active."
  (with-output-to-string (escaped)
    (cond ((and (not escape-leading-tilde)
                (string-prefix-p "~/" text)
                (find-if #'completion--terminal-control-p text))
           (write-string "~/" escaped)
           (completion--write-controlled-text (subseq text 2) escaped))
          ((find-if #'completion--terminal-control-p text)
           (completion--write-controlled-text text escaped))
          (t
           (loop for char across text
                 for index from 0
                 do (when (or (member char *completion-escaped-characters*)
                              (and escape-leading-tilde
                                   (zerop index)
                                   (char= char #\~)))
                      (write-char #\\ escaped))
                    (write-char char escaped))))))

(defun completion--display (text)
  "Render TEXT without terminal controls, retaining a recognizable name."
  (with-output-to-string (display)
    (loop for char across text
          for code = (char-code char)
          do (cond ((< code 32)
                    (write-char #\^ display)
                    (write-char (code-char (+ code 64)) display))
                   ((= code 127)
                    (write-string "^?" display))
                   ((<= 128 code 159)
                    (format display "\\u~4,'0x" code))
                   (t
                    (write-char char display))))))

(defun completion--common-prefix (candidates)
  "Longest safely insertable common prefix of the CANDIDATES."
  (let* ((prefix
           (reduce (lambda (first second)
                     (subseq first 0
                             (or (mismatch first second) (length first))))
                   candidates))
         (control-expression
           (search "$((map 'string #'code-char '" prefix)))
    ;; A partial generated expression cannot be executed, and a lone
    ;; backslash would escape whatever the user types next.
    (when control-expression
      (setf prefix (subseq prefix 0 control-expression)))
    (when (oddp (loop for index downfrom (1- (length prefix)) to 0
                      while (char= (char prefix index) #\\)
                      count 1))
      (setf prefix (subseq prefix 0 (1- (length prefix)))))
    prefix))


;;; Span detection

(defun completion--command-span (buffer cursor)
  "Locate the command mode word being completed at CURSOR.
   Returns (values start prefix command-position-p); START is NIL when
   the cursor sits inside a quoted token, which is not completed."
  (let* ((tokens  (lex-command-line buffer))
         (groups  (token-groups tokens))
         (current nil))
    (dolist (token tokens)
      (when (and (not (eq (token-type token) ':space))
                 (< (token-start token) cursor)
                 (<= cursor (token-end token)))
        (setf current token)))
    (cond ((and current (not (eq (token-type current) ':word)))
           (values nil nil nil))
          (current
           (values (token-start current)
                   (subseq buffer (token-start current) cursor)
                   (eq current (first (first groups)))))
          (t
           (values cursor ""
                   (or (null groups)
                       (>= (token-start (first (first groups))) cursor)))))))

(defun completion--lisp-span (buffer cursor)
  "Locate the Lisp atom being completed at CURSOR.
   Returns (values start prefix)."
  (let ((start cursor))
    (loop while (and (plusp start)
                     (not (lisp-symbol-boundary-p (char buffer (1- start)))))
          do (decf start))
    (values start (subseq buffer start cursor))))


;;; Candidate generation

(defun completion--commands (prefix &key clean-prefix-p)
  "Command name candidates matching PREFIX.
   Returns (values candidates displays)."
  (let* ((clean (if clean-prefix-p prefix (escape-remove prefix)))
         (path-matches
           (coerce (structlisp:sorted-string-index-prefix-items
                    (path-command-names) clean)
                   'list))
         (matches
           (sort (remove-duplicates
                  (append (remove-if-not (lambda (name)
                                           (string-prefix-p clean name))
                                         (shell-command-names))
                          path-matches)
                  :test #'string=)
                 #'string<)))
    (values (mapcar #'completion--escape matches)
            (mapcar #'completion--display matches))))

(defun completion--files
    (prefix &key literal-leading-tilde-p clean-prefix-p)
  "File path candidates matching PREFIX. Directories complete with a
   trailing slash. Returns (values candidates displays).

LITERAL-LEADING-TILDE-P keeps a quoted or explicitly escaped leading tilde
relative instead of interpreting it as the user's home directory. A leading
backslash in the ordinary raw PREFIX is detected automatically.
CLEAN-PREFIX-P says shell escapes have already been removed."
  (let* ((literal-leading-tilde-p
           (or literal-leading-tilde-p
               (and (> (length prefix) 1)
                    (char= (char prefix 0) #\\)
                    (char= (char prefix 1) #\~))))
         (clean          (if clean-prefix-p prefix (escape-remove prefix)))
         (slash          (position #\/ clean :from-end t))
         (directory-part (if slash (subseq clean 0 (1+ slash)) ""))
         (base           (if slash (subseq clean (1+ slash)) clean))
         (list-root      (let ((root (if (string= directory-part "")
                                         "."
                                         directory-part)))
                           (if literal-leading-tilde-p
                               (if (string-prefix-p "~/" root)
                                   (concatenate 'string "./" root)
                                   root)
                               (tilde-expand root))))
         (pairs          nil))
    (flet ((consider (name directory-p)
             (when (and (string-prefix-p base name)
                        (or (not (string-prefix-p "." name))
                            (string-prefix-p "." base)))
               (let* ((tail    (if directory-p "/" ""))
                      (escaped (completion--escape
                                (concatenate 'string directory-part name)
                                :escape-leading-tilde
                                (or literal-leading-tilde-p
                                    (string= directory-part "")))))
                 (push (cons (concatenate 'string escaped tail)
                             (concatenate 'string
                                          (completion--display name)
                                          tail))
                       pairs)))))
      (multiple-value-bind (files subdirectories)
          (directory-entry-names list-root)
        (dolist (file files)
          (consider file nil))
        (dolist (subdirectory subdirectories)
          (consider subdirectory t))))
    (let ((sorted (sort pairs #'string< :key #'cdr)))
      (values (mapcar #'first sorted)
              (mapcar #'rest sorted)))))

(defun completion--command-heads (prefix)
  "Command and directory candidates matching command-position PREFIX.

Directories retain the trailing slash that makes them explicit implicit-cd
paths. Ordinary non-executable files are excluded."
  (multiple-value-bind (commands command-displays)
      (completion--commands prefix)
    (multiple-value-bind (paths path-displays)
        (completion--files prefix)
      (let* ((command-pairs (mapcar #'cons commands command-displays))
             (directory-pairs
               (loop for path in paths
                     for display in path-displays
                     when (and (plusp (length path))
                               (char= (char path (1- (length path))) #\/))
                       collect (cons path display)))
             (pairs
               (sort (remove-duplicates
                      (append command-pairs directory-pairs)
                      :test #'string=
                      :key #'car)
                     #'string<
                     :key #'cdr)))
        (values (mapcar #'car pairs)
                (mapcar #'cdr pairs))))))

(defun completion--symbols (prefix)
  "Lisp symbol candidates matching PREFIX, downcased. Keyword prefixes
   complete against the keyword package."
  (when (plusp (length prefix))
    (let* ((keyword-p (char= (char prefix 0) #\:))
           (lookup    (string-upcase (if keyword-p (subseq prefix 1) prefix)))
           (package   (if keyword-p (find-package '#:keyword) *package*))
           (matches   nil))
      (when (plusp (length lookup))
        (do-symbols (symbol package)
          (let ((name (symbol-name symbol)))
            (when (string-prefix-p lookup name)
              (pushnew (concatenate 'string
                                    (if keyword-p ":" "")
                                    (string-downcase name))
                       matches :test #'string=))))
        (sort matches #'string<)))))


;;; Declarative command arguments

(defstruct (completion-candidate
            (:constructor make-completion-candidate
                (&key insertion display description kind)))
  "One rich semantic completion before the legacy editor boundary.

INSERTION is shell-safe replacement text, DISPLAY is its concise label,
DESCRIPTION is optional explanatory text and KIND identifies the semantic
provider. COMPLETE-LINE deliberately continues to return insertion and display
string lists so existing Clinedi and library callers remain compatible."
  (insertion   "" :type string)
  (display     "" :type string)
  (description nil :type (or null string))
  (kind        nil))

(defun completion--candidate-values (records)
  "Flatten rich completion RECORDS into legacy candidates and displays."
  (values
   (mapcar #'completion-candidate-insertion records)
   (mapcar (lambda (record)
             (completion--described-display
              (completion-candidate-display record)
              (completion-candidate-description record)))
           records)))

(defun completion--candidate-records
    (candidates displays kind &optional description)
  "Pair legacy CANDIDATES and DISPLAYS as rich records of semantic KIND."
  (loop for candidate in candidates
        for display in displays
        collect (make-completion-candidate
                 :insertion candidate
                 :display display
                 :description description
                 :kind kind)))

(defun completion--group-start (group)
  "Return the first source index covered by argument token GROUP."
  (token-start (first group)))

(defun completion--group-end (group)
  "Return the exclusive source index covered by argument token GROUP."
  (token-end (first (last group))))

(defun completion--group-at-cursor (groups cursor)
  "Return the argument token group containing CURSOR, or NIL.

The exclusive end of a group remains part of it for completion purposes, so
Tab immediately after a word completes that word instead of starting another."
  (find-if (lambda (group)
             (and (<= (completion--group-start group) cursor)
                  (<= cursor (completion--group-end group))))
           groups))

(defun completion--group-value (buffer group)
  "Return GROUP's safely expanded string and whether its argv size is known.

Environment variables, tilde notation and quotes may be expanded because they
do not run user code. Lisp substitutions and an unquoted glob can produce an
unknown number of words, so they make the second value false."
  (when (find ':lisp group :key #'token-type)
    (return-from completion--group-value (values nil nil)))
  (when (and (null (rest group))
             (eq (token-type (first group)) ':word))
    (let* ((text (token-text buffer (first group)))
           (expanded
             (variable-expand (tilde-expand text) :keep-escapes t)))
      ;; A variable can introduce a wildcard even when the source token has
      ;; none. Execution may then produce several argv entries, so semantic
      ;; completion must not guess the following positional index.
      (when (glob-pattern-p expanded)
        (return-from completion--group-value (values nil nil)))))
  (handler-case
      (values
       (apply #'concatenate 'string
              (mapcar (lambda (token)
                        (token-expand buffer token))
                      group))
       t)
    (serious-condition ()
      (values nil nil))))

(defun completion--groups-before-cursor (groups current cursor)
  "Return complete argument GROUPS preceding CURRENT at CURSOR."
  (loop for group in groups
        until (eq group current)
        while (<= (completion--group-end group) cursor)
        collect group))

(defun completion--group-prefix (buffer group cursor)
  "Return GROUP's semantic prefix, replacement start and usability flag.

A quoted or compound group is completed only at its end, where replacing the
whole expression with a freshly escaped candidate cannot leave unmatched
syntax behind. Completion inside any group is conservative because Clinedi
replaces only the text through CURSOR and would otherwise retain a stale
suffix. The fourth value says whether a leading tilde was quoted or escaped."
  (cond ((and group (< cursor (completion--group-end group)))
         (values nil cursor nil nil))
        ((null group)
         (values "" cursor t nil))
        ((and (null (rest group))
              (eq (token-type (first group)) ':word))
         (let* ((start (token-start (first group)))
                (raw   (subseq buffer start cursor)))
           (handler-case
               (values (escape-remove raw)
                       start
                       t
                       (and (> (length raw) 1)
                            (char= (char raw 0) #\\)
                            (char= (char raw 1) #\~)))
             (serious-condition ()
               (values nil cursor nil nil)))))
        ((= cursor (completion--group-end group))
         (multiple-value-bind (value certain-p)
             (completion--group-value buffer group)
           (let ((start (completion--group-start group)))
             (values value start certain-p
                     (and (< start (length buffer))
                          (member (char buffer start) '(#\" #\'))
                          t)))))
        (t
         (values nil cursor nil nil))))

(defun completion--option-long-name (argument)
  "Return ARGUMENT's declared long option name."
  (let ((name (command-argument-long-name argument)))
    (when name
      (princ-to-string name))))

(defun completion--option-arguments (command)
  "Return COMMAND's declarative option arguments in definition order."
  (remove-if-not (lambda (argument)
                   (eq (command-argument-kind argument) ':option))
                 (command-arguments command)))

(defun completion--long-option (options name)
  "Find the option in OPTIONS whose long name is NAME."
  (find-if (lambda (argument)
             (let ((long-name (completion--option-long-name argument)))
               (and long-name (string= name long-name))))
           options))

(defun completion--short-option (options name)
  "Find the option in OPTIONS whose short name is character NAME."
  (find-if (lambda (argument)
             (let ((short-name (command-argument-short-name argument)))
               (and short-name (char= name short-name))))
           options))

(defun completion--boolean-option-p (argument)
  "True when ARGUMENT is a flag that consumes no value."
  (eq (command-argument-value-type argument) ':boolean))

(defun completion--argument-description (argument)
  "Return ARGUMENT's one-line documentation, or NIL."
  (let ((documentation (command-argument-documentation argument)))
    (when documentation
      (subseq documentation 0 (or (position #\newline documentation)
                                  (length documentation))))))

(defun completion--described-display (label description)
  "Return a safe completion display combining LABEL and DESCRIPTION."
  (let ((safe-label (completion--display label)))
    (if (and description (plusp (length description)))
        (format nil "~a  ~a" safe-label (completion--display description))
        safe-label)))

(defun completion--option-spellings (argument)
  "Return the short and long command-line spellings for ARGUMENT."
  (let ((short (command-argument-short-name argument))
        (long  (completion--option-long-name argument)))
    (append (and short (list (format nil "-~c" short)))
            (and long (list (concatenate 'string "--" long))))))

(defun completion--option-candidates (command prefix used-options)
  "Return option candidates and descriptive displays matching PREFIX."
  (let ((records nil))
    (dolist (argument (completion--option-arguments command))
      (when (or (command-argument-repeating-p argument)
                (not (member argument used-options :test #'eq)))
        (let* ((spellings   (completion--option-spellings argument))
               (description (completion--argument-description argument))
               (aliases     (format nil "~{~a~^, ~}" spellings)))
          (dolist (spelling spellings)
            (when (string-prefix-p prefix spelling)
              (push (make-completion-candidate
                     :insertion spelling
                     :display aliases
                     :description description
                     :kind ':option)
                    records))))))
    (when (string-prefix-p prefix "--help")
      (push (make-completion-candidate
             :insertion "--help"
             :display "--help"
             :description "Show command help."
             :kind ':help)
            records))
    (setf records
          (sort (remove-duplicates
                 records :test #'string=
                         :key #'completion-candidate-insertion)
                #'string< :key #'completion-candidate-insertion))
    (completion--candidate-values records)))

(defun completion--directory-candidates
    (prefix &key literal-leading-tilde-p description clean-prefix-p)
  "Return only directory path candidates matching PREFIX."
  (multiple-value-bind (candidates displays)
      (completion--files
       prefix
       :literal-leading-tilde-p literal-leading-tilde-p
       :clean-prefix-p clean-prefix-p)
    (completion--candidate-values
     (loop for candidate in candidates
           for display in displays
           when (and (plusp (length candidate))
                     (char= (char candidate (1- (length candidate))) #\/))
             collect (make-completion-candidate
                      :insertion candidate
                      :display display
                      :description description
                      :kind ':directory)))))

(defun completion--package-candidates (prefix &optional description)
  "Return package-name and nickname candidates matching PREFIX."
  (let ((records nil))
    (dolist (package (list-all-packages))
      (dolist (name (cons (package-name package) (package-nicknames package)))
        (let ((candidate (string-downcase name)))
          (when (and (string-prefix-p (string-downcase prefix) candidate)
                     (not (find candidate records :test #'string=
                                :key #'completion-candidate-display)))
            (push (make-completion-candidate
                   :insertion (completion--escape candidate)
                   :display candidate
                   :description description
                   :kind ':package)
                  records)))))
    (setf records
          (sort records #'string< :key #'completion-candidate-display))
    (completion--candidate-values records)))

(defun completion--environment-names ()
  "Return sorted environment-variable names without exposing their values."
  (sort
   (remove-duplicates
    (loop for entry in (environment-variables)
          for equals = (position #\= entry)
          when equals
            collect (subseq entry 0 equals))
    :test #'string=)
   #'string<))

(defun completion--environment-candidates (prefix &optional description)
  "Return environment-variable name candidates matching PREFIX."
  (completion--candidate-values
   (loop for name in (completion--environment-names)
         when (string-prefix-p prefix name)
           collect (make-completion-candidate
                    :insertion (completion--escape name)
                    :display name
                    :description description
                    :kind ':environment-variable))))

(defun completion--job-description (job)
  "Return a single-line status and command description for JOB."
  (format nil "~:(~a~)  ~a" (job-status job) (job-command job)))

(defun completion--job-candidates (prefix &optional description)
  "Return current job selectors matching PREFIX with useful descriptions."
  (let* ((ordered (jobs--ordered))
         (current (first ordered))
         (previous (second ordered))
         (records nil))
    (labels ((consider (spelling job)
               (when (and job (string-prefix-p prefix spelling))
                 (push (make-completion-candidate
                        :insertion (completion--escape spelling)
                        :display spelling
                        :description (or (completion--job-description job)
                                         description)
                        :kind ':job)
                       records))))
      (consider "%+" current)
      (consider "%-" previous)
      (dolist (job (sort (copy-list *jobs*) #'< :key #'job-id))
        (consider (format nil "%~d" (job-id job)) job)))
    (setf records
          (sort (remove-duplicates
                 records :test #'string=
                         :key #'completion-candidate-insertion)
                #'string< :key #'completion-candidate-insertion))
    (completion--candidate-values records)))

(defun completion--provider-values
    (values descriptions prefix kind &optional fallback-description)
  "Turn raw provider VALUES into escaped candidates matching PREFIX.

DESCRIPTIONS, when supplied, runs parallel to VALUES. Non-string values are
printed readably enough for shell argument insertion."
  (let* ((items (cond ((null values) nil)
                      ((stringp values) (list values))
                      ((typep values 'sequence) (coerce values 'list))
                      (t (list values))))
         (notes (cond ((null descriptions) nil)
                      ((stringp descriptions) (list descriptions))
                      ((typep descriptions 'sequence)
                       (coerce descriptions 'list))
                      (t (list descriptions))))
         (records nil))
    (loop for item in items
          for note = (and notes (pop notes))
          do (if (completion-candidate-p item)
                 (when (string-prefix-p
                        prefix
                        (escape-remove
                         (completion-candidate-insertion item)))
                   (push (make-completion-candidate
                          :insertion
                          (completion-candidate-insertion item)
                          :display
                          (completion-candidate-display item)
                          :description
                          (or (completion-candidate-description item)
                              (and note (princ-to-string note))
                              fallback-description)
                          :kind (or (completion-candidate-kind item) kind))
                         records))
                 (let ((raw (command--argument-value-string item)))
                   (when (string-prefix-p prefix raw)
                     (push (make-completion-candidate
                            :insertion (completion--escape raw)
                            :display raw
                            :description (or (and note (princ-to-string note))
                                             fallback-description)
                            :kind kind)
                           records)))))
    (setf records
          (sort (remove-duplicates
                 records :test #'string=
                         :key #'completion-candidate-insertion)
                #'string< :key #'completion-candidate-insertion))
    (completion--candidate-values records)))

(defun completion--call-provider (argument context)
  "Call ARGUMENT's custom completion provider and contain its failures."
  (handler-case
      (multiple-value-bind (values descriptions)
          (funcall (command-argument-completion-function argument)
                   argument context)
        (completion--provider-values
         values descriptions (command-completion-context-prefix context)
         (let ((kind (command-argument-value-type argument)))
           (if (keywordp kind) kind ':custom))
         (completion--argument-description argument)))
    (serious-condition ()
      (values nil nil))))

(defun completion--choice-candidates (argument context)
  "Return static or dynamic choice candidates for ARGUMENT and CONTEXT."
  (handler-case
      (completion--provider-values
       (command-argument-choice-values argument context)
       nil
       (command-completion-context-prefix context)
       ':choice
       (completion--argument-description argument))
    (serious-condition ()
      (values nil nil))))

(defun completion--argument-candidates
    (argument context &key literal-leading-tilde-p)
  "Return semantic candidates and displays for ARGUMENT in CONTEXT."
  (cond ((command-argument-completion-function argument)
         (completion--call-provider argument context))
        ((or (command-argument-choices argument)
             (eq (command-argument-value-type argument) ':choice))
         (completion--choice-candidates argument context))
        ((member (command-argument-value-type argument) '(:path :pathname))
         (multiple-value-bind (candidates displays)
             (completion--files
              (command-completion-context-prefix context)
              :literal-leading-tilde-p literal-leading-tilde-p
              :clean-prefix-p t)
           (completion--candidate-values
            (completion--candidate-records
             candidates displays ':path
             (completion--argument-description argument)))))
        ((eq (command-argument-value-type argument) ':directory)
         (completion--directory-candidates
          (command-completion-context-prefix context)
          :literal-leading-tilde-p literal-leading-tilde-p
          :description (completion--argument-description argument)
          :clean-prefix-p t))
        ((eq (command-argument-value-type argument) ':command)
         (multiple-value-bind (candidates displays)
             (completion--commands
              (command-completion-context-prefix context)
              :clean-prefix-p t)
           (completion--candidate-values
            (completion--candidate-records
             candidates displays ':command
             (completion--argument-description argument)))))
        ((eq (command-argument-value-type argument) ':package)
         (completion--package-candidates
          (command-completion-context-prefix context)
          (completion--argument-description argument)))
        ((member (command-argument-value-type argument)
                 '(:environment :environment-variable))
         (completion--environment-candidates
          (command-completion-context-prefix context)
          (completion--argument-description argument)))
        ((eq (command-argument-value-type argument) ':job)
         (completion--job-candidates
          (command-completion-context-prefix context)
          (completion--argument-description argument)))
        (t
         (values nil nil))))

(defun completion--active-short-value (options prefix)
  "Return an attached value option, value start and preceding flag options.

The second value is the character index after the value-taking option. NIL
means PREFIX is not an attached short option value."
  (let ((used nil))
    (when (and (> (length prefix) 1)
               (char= (char prefix 0) #\-)
               (not (string-prefix-p "--" prefix)))
      (loop for index from 1 below (length prefix)
            for option = (completion--short-option options (char prefix index))
            do (unless option
                 (return (values nil nil nil)))
               (if (completion--boolean-option-p option)
                   (push option used)
                   (return (values option (1+ index) (nreverse used))))))))

(defun completion--positional-prefix-p (argument prefix)
  "True when PREFIX has execution's positional treatment despite leading -."
  (and argument
       (eq (command-argument-kind argument) ':positional)
       (or (string= prefix "-")
           (and (member (command-argument-value-type argument)
                        '(:integer :number))
                (command--read-number prefix)
                t))))

(defun completion--literal-leading-tilde-source-p (buffer start cursor)
  "True when source text from START quotes or escapes a possible tilde."
  (and (< start cursor)
       (or (member (char buffer start) '(#\" #\'))
           (and (< (1+ start) cursor)
                (char= (char buffer start) #\\)
                (char= (char buffer (1+ start)) #\~)))
       t))

(defun completion--argument-context
    (command argument words used-options positional-index prefix buffer cursor)
  "Construct the callback context for one active declarative ARGUMENT."
  (make-command-completion-context
   :command command
   :argument argument
   :words words
   :used-options used-options
   :positional-index positional-index
   :prefix prefix
   :buffer buffer
   :cursor cursor))

(defun completion--declared-arguments (buffer cursor groups current)
  "Complete a declarative builtin argument at CURSOR.

Return START, candidates, displays and a handled flag. A false handled flag
asks the caller to retain legacy file completion for external commands and
builtins without an argument declaration."
  (let ((before (completion--groups-before-cursor groups current cursor)))
    (when (null before)
      (return-from completion--declared-arguments
        (values cursor nil nil nil)))
    (multiple-value-bind (head certain-p)
        (completion--group-value buffer (first before))
      (unless certain-p
        (return-from completion--declared-arguments
          (values cursor nil nil nil)))
      (multiple-value-bind (kind command)
          (command-resolve head)
        (unless (and (eq kind ':builtin)
                     (command-declarative-arguments-p command))
          (return-from completion--declared-arguments
            (values cursor nil nil nil)))
        (let ((words nil))
          (dolist (group (rest before))
            (multiple-value-bind (word known-size-p)
                (completion--group-value buffer group)
              (unless known-size-p
                (return-from completion--declared-arguments
                  (values cursor nil nil t)))
              (push word words)))
          (setf words (nreverse words))
          (when (command--help-request-p command words)
            (return-from completion--declared-arguments
              (values cursor nil nil t)))
          (multiple-value-bind
                (prefix start usable-p literal-leading-tilde-p)
              (completion--group-prefix buffer current cursor)
            (unless usable-p
              (return-from completion--declared-arguments
                (values cursor nil nil t)))
            (let ((context
                    (handler-case
                        (command-arguments-context
                         command words
                         :prefix prefix :buffer buffer :cursor cursor)
                      (serious-condition () nil))))
              (unless context
                (return-from completion--declared-arguments
                  (values cursor nil nil t)))
              (let* ((options
                       (completion--option-arguments command))
                     (options-enabled
                       (command-completion-context-options-enabled-p context))
                     (used-options
                       (command-completion-context-used-options context))
                     (positional-index
                       (command-completion-context-positional-index context))
                     (active-argument
                       (command-completion-context-argument context))
                     (pending-option-p
                       (and active-argument
                            (eq (command-argument-kind active-argument)
                                ':option)))
                     (long-equals
                       (and options-enabled
                            (string-prefix-p "--" prefix)
                            (position #\= prefix :start 2)))
                     (long-option
                       (and long-equals
                            (completion--long-option
                             options (subseq prefix 2 long-equals)))))
                (cond
                  (pending-option-p
                   (multiple-value-bind (candidates displays)
                       (completion--argument-candidates
                        active-argument context
                        :literal-leading-tilde-p literal-leading-tilde-p)
                     (values start candidates displays t)))
                  ((and long-option
                        (not (completion--boolean-option-p long-option)))
                   (let* ((source-equals
                            (position #\= buffer :start start :end cursor))
                          (value-prefix (subseq prefix (1+ long-equals)))
                          (value-start (if source-equals
                                           (1+ source-equals)
                                           (+ start long-equals 1)))
                          (value-context
                            (completion--argument-context
                             command long-option words used-options
                             positional-index value-prefix buffer cursor)))
                     (multiple-value-bind (candidates displays)
                         (completion--argument-candidates
                          long-option value-context
                          :literal-leading-tilde-p
                          (completion--literal-leading-tilde-source-p
                           buffer value-start cursor))
                       (values value-start candidates displays t))))
                  ((completion--positional-prefix-p active-argument prefix)
                   (multiple-value-bind (candidates displays)
                       (completion--argument-candidates
                        active-argument context
                        :literal-leading-tilde-p literal-leading-tilde-p)
                     (values start candidates displays t)))
                  ((and options-enabled
                        (string-prefix-p "-" prefix))
                   (multiple-value-bind
                         (short-option value-index short-used-options)
                       (completion--active-short-value options prefix)
                     (if short-option
                         (let* ((value-prefix (subseq prefix value-index))
                                (value-start (+ start value-index))
                                (value-context
                                  (completion--argument-context
                                   command short-option words
                                   (append used-options short-used-options)
                                   positional-index value-prefix buffer cursor)))
                           (multiple-value-bind (candidates displays)
                               (completion--argument-candidates
                                short-option value-context
                                :literal-leading-tilde-p
                                (completion--literal-leading-tilde-source-p
                                 buffer value-start cursor))
                             (values value-start candidates displays t)))
                         (multiple-value-bind (candidates displays)
                             (completion--option-candidates
                              command prefix used-options)
                           (values start candidates displays t)))))
                  (active-argument
                   (multiple-value-bind (candidates displays)
                       (completion--argument-candidates
                        active-argument context
                        :literal-leading-tilde-p literal-leading-tilde-p)
                     (values start candidates displays t)))
                  (t
                   (values start nil nil t)))))))))))


;;; Entry point

(defun completion--token-at (tokens cursor)
  "The non-space token containing CURSOR, or NIL."
  (let ((found nil))
    (dolist (token tokens found)
      (when (and (not (eq (token-type token) ':space))
                 (< (token-start token) cursor)
                 (<= cursor (token-end token)))
        (setf found token)))))

(defun completion--lisp-matches (buffer cursor)
  "Symbol completion at CURSOR, shared by Lisp mode and :lisp
   substitutions inside command lines."
  (multiple-value-bind (start prefix)
      (completion--lisp-span buffer cursor)
    (let ((matches (completion--symbols prefix)))
      (values start matches matches))))

(defun complete-line (buffer cursor)
  "Compute completions at CURSOR. Returns (values start candidates
   displays); each candidate replaces BUFFER between START and CURSOR."
  (when (line-comment-p buffer)
    (return-from complete-line (values cursor nil nil)))
  (if (line-lisp-p buffer)
      (completion--lisp-matches buffer cursor)
      (let ((current (completion--token-at (lex-command-line buffer) cursor)))
        (if (and current (eq (token-type current) ':lisp))
            (completion--lisp-matches buffer cursor)
            (multiple-value-bind (start prefix command-position-p)
                (completion--command-span buffer cursor)
              (cond ((null start)
                     (let* ((tokens  (lex-command-line buffer))
                            (groups  (token-groups tokens))
                            (current-group
                              (completion--group-at-cursor groups cursor)))
                       (multiple-value-bind
                             (argument-start candidates displays handled-p)
                           (completion--declared-arguments
                            buffer cursor groups current-group)
                         (if handled-p
                             (values argument-start candidates displays)
                             (values cursor nil nil)))))
                    ((and command-position-p (not (find #\/ prefix)))
                     (completion--commands-with-start start prefix))
                    (t
                     (let* ((tokens  (lex-command-line buffer))
                            (groups  (token-groups tokens))
                            (current-group
                              (completion--group-at-cursor groups cursor)))
                       (multiple-value-bind
                             (argument-start candidates displays handled-p)
                           (completion--declared-arguments
                            buffer cursor groups current-group)
                         (if handled-p
                             (values argument-start candidates displays)
                             (multiple-value-bind
                                   (file-candidates file-displays)
                                 (completion--files prefix)
                               (values start
                                       file-candidates
                                       file-displays))))))))))))

(defun completion--commands-with-start (start prefix)
  "Command and directory candidates wrapped with replacement START."
  (multiple-value-bind (candidates displays)
      (completion--command-heads prefix)
    (values start candidates displays)))
