;;;; -- Commands --
;;;
;;; The COMMAND class, builtin definition, PATH lookup and builtin
;;; execution. External programs run through jobs.lisp.

(in-package #:cclsh)

(defvar *last-status* 0
  "Exit status of the last executed command.")

(defvar *lisp-dispatch-status-cell* nil
  "Dynamically bound while a Lisp input line is evaluated. Shell
   helpers record their status in the cell so successful Lisp dispatch
   does not replace it with zero.")

(defun command-status-record (status)
  "Record STATUS as the last shell status and return it. When called
   from Lisp dispatch, also preserve STATUS as that dispatch's result."
  (setf *last-status* status)
  (when *lisp-dispatch-status-cell*
    (setf (first *lisp-dispatch-status-cell*) status))
  status)

(defvar *path-cache* (make-hash-table :test #'equal)
  "Cache of PATH lookups: command name to namestring or NIL.")

(defvar *path-cache-source* nil
  "The PATH value *PATH-CACHE* was built against.")

(defvar *path-command-names* nil
  "Cached sorted string index of executable names found in PATH, for
   prefix completion.")

(defvar *path-command-names-source* nil
  "The PATH value *PATH-COMMAND-NAMES* was built against.")


;;; Conditions

(define-condition shell-error (error)
  ()
  (:documentation "Base condition for cclsh failures."))

(define-condition command-not-found-error (shell-error)
  ((name :initarg :name :reader command-not-found-name))
  (:documentation "Signaled when a command word resolves to nothing.")
  (:report (lambda (condition stream)
             (format stream "command not found: ~a"
                     (command-not-found-name condition)))))

(define-condition command-argument-error (shell-error)
  ((command :initarg :command :reader command-argument-error-command)
   (message :initarg :message :reader command-argument-error-message))
  (:documentation "Signaled when declared command arguments are invalid.")
  (:report
   (lambda (condition stream)
     (format stream "~(~a~): ~a"
             (command-name (command-argument-error-command condition))
             (command-argument-error-message condition)))))


;;; Command metadata

(defclass command-argument ()
  ((name                :initarg :name
                        :reader command-argument-name)
   (kind                :initarg :kind
                        :reader command-argument-kind)
   (value-type          :initarg :value-type
                        :reader command-argument-value-type
                        :initform ':string)
   (required-p          :initarg :required-p
                        :reader command-argument-required-p
                        :initform nil)
   (repeating-p         :initarg :repeating-p
                        :reader command-argument-repeating-p
                        :initform nil)
   (short-name          :initarg :short-name
                        :reader command-argument-short-name
                        :initform nil)
   (long-name           :initarg :long-name
                        :reader command-argument-long-name
                        :initform nil)
   (documentation       :initarg :documentation
                        :reader command-argument-documentation
                        :initform nil)
   (choices             :initarg :choices
                        :reader command-argument-choices
                        :initform nil)
   (completion-function :initarg :completion-function
                        :reader command-argument-completion-function
                        :initform nil)
   (metavariable        :initarg :metavariable
                        :reader command-argument-metavariable
                        :initform nil)
   (convert-p           :initarg :convert-p
                        :reader command-argument-convert-p
                        :initform nil)
   (converter           :initarg :converter
                        :reader command-argument-converter
                        :initform nil)
   (keyword             :initarg :keyword
                        :reader command--argument-keyword
                        :initform nil))
  (:documentation
   "Declarative metadata for one positional or option command argument."))

(defclass command-completion-context ()
  ((command          :initarg :command
                     :reader command-completion-context-command)
   (argument         :initarg :argument
                     :reader command-completion-context-argument
                     :initform nil)
   (words            :initarg :words
                     :reader command-completion-context-words
                     :initform nil)
   (used-options     :initarg :used-options
                     :reader command-completion-context-used-options
                     :initform nil)
   (positional-index :initarg :positional-index
                     :reader command-completion-context-positional-index
                     :initform 0)
   (options-enabled-p :initarg :options-enabled-p
                      :reader command-completion-context-options-enabled-p
                      :initform t)
   (prefix           :initarg :prefix
                     :reader command-completion-context-prefix
                     :initform "")
   (buffer           :initarg :buffer
                     :reader command-completion-context-buffer
                     :initform nil)
   (cursor           :initarg :cursor
                     :reader command-completion-context-cursor
                     :initform nil))
  (:documentation
   "Stable context passed to dynamic choice and completion functions."))

(defun make-command-completion-context
    (&key command argument words used-options positional-index
          (options-enabled-p t) prefix buffer cursor)
  "Construct a completion context for COMMAND-ARGUMENT callbacks.
WORDS are complete raw argument words preceding PREFIX. USED-OPTIONS is a
list of COMMAND-ARGUMENT objects and POSITIONAL-INDEX counts positionals
already consumed. OPTIONS-ENABLED-P is false after a parsed -- terminator.
BUFFER and CURSOR may be NIL outside interactive editing."
  (make-instance 'command-completion-context
                 :command          command
                 :argument         argument
                 :words            words
                 :used-options     used-options
                 :positional-index (or positional-index 0)
                 :options-enabled-p options-enabled-p
                 :prefix           (or prefix "")
                 :buffer           buffer
                 :cursor           cursor))

(defun command-argument-choice-values (argument &optional context)
  "Return the choices declared for ARGUMENT.
Static choices are returned directly. A dynamic choice function is called as
(FUNCTION ARGUMENT CONTEXT), using the same callback contract as custom
completion functions. Callback conditions are deliberately left visible so
the execution or completion boundary can report or contain them."
  (let ((choices (command-argument-choices argument)))
    (cond ((null choices)
           nil)
          ((or (functionp choices)
               (and (symbolp choices) (fboundp choices)))
           (funcall choices argument context))
          ((listp choices)
           choices)
          (t
           (error "Invalid choices for command argument ~a: ~s"
                  (command-argument-name argument) choices)))))


;;; The COMMAND class

(defclass command ()
  ((name                  :initarg :name
                          :reader command-name)
   (function              :initarg :function
                          :reader command-function)
   (documentation         :initarg :documentation
                          :reader command-documentation
                          :initform nil)
   (arguments             :initarg :arguments
                          :reader command-arguments
                          :initform nil)
   (declarative-arguments :initarg :declarative-arguments-p
                          :reader command-declarative-arguments-p
                          :initform nil)
   (invoker               :initarg :invoker
                          :reader command--invoker
                          :initform nil))
  (:documentation "A shell command implemented in Lisp."))

(defmethod initialize-instance :after ((command command) &key)
  "Validate option spellings after constructing COMMAND."
  (let ((long-names  (make-hash-table :test #'equal))
        (short-names (make-hash-table :test #'eql)))
    (dolist (argument (command-arguments command))
      (unless (stringp (command-argument-metavariable argument))
        (error "Command ~(~a~) argument ~a has invalid metavariable ~s"
               (command-name command)
               (command-argument-name argument)
               (command-argument-metavariable argument)))
      (when (eq (command-argument-kind argument) ':option)
        (let ((long  (command-argument-long-name argument))
              (short (command-argument-short-name argument)))
          (unless (or long short)
            (error "Command ~(~a~) option ~a has no spelling"
                   (command-name command)
                   (command-argument-name argument)))
          (when (and long
                     (or (not (stringp long))
                         (zerop (length long))
                         (char= (char long 0) #\-)
                         (find #\= long)
                         (find-if
                          (lambda (character)
                            (member character
                                    '(#\Space #\Tab #\Newline #\Return)))
                          long)))
            (error "Command ~(~a~) option ~a has invalid long spelling ~s"
                   (command-name command)
                   (command-argument-name argument)
                   long))
          (when (and short
                     (or (not (characterp short))
                         (char= short #\-)
                         (member short
                                 '(#\Space #\Tab #\Newline #\Return))))
            (error "Command ~(~a~) option ~a has invalid short spelling ~s"
                   (command-name command)
                   (command-argument-name argument)
                   short))
          (when (and (command-declarative-arguments-p command)
                     long
                     (string= long "help"))
            (error "Command ~(~a~) reserves --help for generated help"
                   (command-name command)))
          (when long
            (when (gethash long long-names)
              (error "Command ~(~a~) declares --~a more than once"
                     (command-name command) long))
            (setf (gethash long long-names) t))
          (when short
            (when (gethash short short-names)
              (error "Command ~(~a~) declares -~c more than once"
                     (command-name command) short))
            (setf (gethash short short-names) t)))))))

(defstruct (command--parameter
            (:constructor command--make-parameter
                (&key name role initform supplied-name keyword)))
  "One normalized ordinary function lambda-list parameter."
  name
  role
  initform
  supplied-name
  keyword)

(defun command--ordinary-parameter (entry role)
  "Normalize required, optional, rest or auxiliary lambda-list ENTRY."
  (ecase role
    (:required
     (unless (symbolp entry)
       (error "DEFCOMMAND required parameter is not a symbol: ~s" entry))
     (command--make-parameter :name entry :role role))
    (:optional
     (let ((name (if (consp entry) (first entry) entry)))
       (unless (symbolp name)
         (error "DEFCOMMAND optional parameter is not a symbol: ~s" entry))
       (command--make-parameter
        :name          name
        :role          role
        :initform      (if (consp entry) (second entry) nil)
        :supplied-name (and (consp entry) (third entry)))))
    (:rest
     (unless (symbolp entry)
       (error "DEFCOMMAND rest parameter is not a symbol: ~s" entry))
     (command--make-parameter :name entry :role role))
    (:aux
     (let ((name (if (consp entry) (first entry) entry)))
       (unless (symbolp name)
         (error "DEFCOMMAND auxiliary parameter is not a symbol: ~s" entry))
       (command--make-parameter
        :name     name
        :role     role
        :initform (and (consp entry) (second entry)))))))

(defun command--keyword-parameter (entry)
  "Normalize one &KEY lambda-list ENTRY."
  (let* ((head          (if (consp entry) (first entry) entry))
         (explicit-p    (consp head))
         (name          (if explicit-p (second head) head))
         (keyword       (if explicit-p
                            (first head)
                            (intern (symbol-name name) '#:keyword)))
         (initform      (and (consp entry) (second entry)))
         (supplied-name (and (consp entry) (third entry))))
    (unless (and (symbolp name) (symbolp keyword))
      (error "DEFCOMMAND keyword parameter is malformed: ~s" entry))
    (command--make-parameter
     :name          name
     :role          ':key
     :initform      initform
     :supplied-name supplied-name
     :keyword       keyword)))

(defun command--lambda-parameters (lambda-list)
  "Return normalized input and auxiliary parameters from LAMBDA-LIST."
  (let ((state      ':required)
        (parameters nil)
        (rest       nil))
    (dolist (entry lambda-list)
      (cond ((eq entry '&optional)
             (setf state ':optional))
            ((member entry '(&rest &body))
             (setf state ':rest-marker))
            ((eq entry '&key)
             (when rest
               (setf (command--parameter-role rest) ':keyword-rest))
             (setf state ':key))
            ((eq entry '&allow-other-keys)
             (unless (eq state ':key)
               (error "DEFCOMMAND has &ALLOW-OTHER-KEYS outside &KEY")))
            ((eq entry '&aux)
             (setf state ':aux))
            ((and (symbolp entry)
                  (plusp (length (symbol-name entry)))
                  (char= (char (symbol-name entry) 0) #\&))
             (error "DEFCOMMAND does not support lambda-list marker ~s"
                    entry))
            ((eq state ':rest-marker)
             (setf rest (command--ordinary-parameter entry ':rest))
             (push rest parameters)
             (setf state ':after-rest))
            ((eq state ':after-rest)
             (error "DEFCOMMAND has a parameter after &REST without &KEY"))
            ((eq state ':key)
             (push (command--keyword-parameter entry) parameters))
            (t
             (push (command--ordinary-parameter entry state) parameters))))
    (when (eq state ':rest-marker)
      (error "DEFCOMMAND has no parameter after &REST"))
    (nreverse parameters)))

(defun command--plist-value (plist key)
  "Return PLIST's value for KEY and whether KEY was present."
  (loop for tail on plist by #'cddr
        when (eq (first tail) key)
          return (values (second tail) t)
        finally (return (values nil nil))))

(defparameter +command-argument-specification-keys+
  '(:type :required :short :long :help :choices :completion
    :metavariable :convert :converter)
  "Keywords accepted in a DEFCOMMAND :ARGUMENTS specification.")

(defun command--argument-specifications (declaration parameters)
  "Validate DECLARATION and return its specifications by parameter name."
  (let ((specifications nil)
        (input-names
          (loop for parameter in parameters
                unless (member (command--parameter-role parameter)
                               '(:aux :keyword-rest))
                  collect (command--parameter-name parameter))))
    (dolist (specification (rest declaration))
      (unless (and (consp specification)
                   (symbolp (first specification))
                   (evenp (length (rest specification))))
        (error "Malformed DEFCOMMAND argument specification: ~s"
               specification))
      (let ((name  (first specification))
            (plist (rest specification))
            (seen  nil))
        (unless (member name input-names :test #'eq)
          (error "DEFCOMMAND argument ~s is not in its lambda list" name))
        (when (assoc name specifications :test #'eq)
          (error "Duplicate DEFCOMMAND argument specification for ~s" name))
        (loop for tail on plist by #'cddr
              for key = (first tail)
              do (unless (member key +command-argument-specification-keys+)
                   (error "Unknown DEFCOMMAND argument property ~s" key))
                 (when (member key seen)
                   (error "Duplicate DEFCOMMAND argument property ~s" key))
                 (push key seen))
        (push (cons name plist) specifications)))
    (nreverse specifications)))

(defun command--short-name (value name)
  "Normalize a short option VALUE declared for argument NAME."
  (cond ((null value)
         nil)
        ((characterp value)
         value)
        ((and (stringp value) (= (length value) 1))
         (char value 0))
        (t
         (error "Short option for ~s must be one character, not ~s"
                name value))))

(defun command--literal-form (value)
  "Return a macro expansion form that produces declarative VALUE."
  (cond ((and (consp value) (member (first value) '(quote function)))
         value)
        ((or (keywordp value) (symbolp value) (consp value))
         `',value)
        (t
         value)))

(defun command--function-designator-form-p (value)
  "True when VALUE can declare a callback without evaluating it now."
  (or (null value)
      (symbolp value)
      (functionp value)
      (and (consp value)
           (eq (first value) 'function))))

(defun command--parameter-argument-form (parameter specification)
  "Return the COMMAND-ARGUMENT constructor form for PARAMETER."
  (let* ((plist       (rest specification))
         (role        (command--parameter-role parameter))
         (option-p    (eq role ':key))
         (required-p  (eq role ':required))
         (repeating-p (eq role ':rest))
         (name        (command--parameter-name parameter)))
    (multiple-value-bind (specified-required required-present-p)
        (command--plist-value plist ':required)
      (multiple-value-bind (specified-type type-present-p)
          (command--plist-value plist ':type)
        (multiple-value-bind (choices choices-present-p)
            (command--plist-value plist ':choices)
          (let* ((value-type
                   (cond (type-present-p specified-type)
                         (choices-present-p ':choice)
                         (t ':string)))
                 (short
                   (command--short-name (getf plist ':short) name))
                 (long
                   (if option-p
                       (multiple-value-bind (specified present-p)
                           (command--plist-value plist ':long)
                         (if present-p
                             specified
                             (string-downcase
                              (symbol-name
                               (command--parameter-keyword parameter)))))
                       nil))
                 (metavariable-present-p
                   (nth-value
                    1 (command--plist-value plist ':metavariable)))
                 (metavariable
                   (if metavariable-present-p
                       (getf plist ':metavariable)
                       (string-upcase (symbol-name name)))))
            (when (and (not option-p)
                       (or short (getf plist ':long)))
              (error "Positional DEFCOMMAND argument ~s has option names"
                     name))
            (when (and long (not (stringp long)))
              (error "Long option for ~s must be a string or NIL" name))
            (when (and (getf plist ':help)
                       (not (stringp (getf plist ':help))))
              (error "Help for DEFCOMMAND argument ~s must be a string"
                     name))
            (when (and required-present-p
                       (not (member specified-required '(nil t))))
              (error ":REQUIRED for ~s must be NIL or T" name))
            (when (and (eq role ':required)
                       required-present-p
                       (null specified-required))
              (error "Required lambda parameter ~s cannot be optional"
                     name))
            (when (not (stringp metavariable))
              (error ":METAVARIABLE for ~s must be a string" name))
            (dolist (property '(:completion :converter))
              (multiple-value-bind (callback present-p)
                  (command--plist-value plist property)
                (when (and present-p
                           (not (command--function-designator-form-p
                                 callback)))
                  (error "~s for ~s must be a function designator"
                         property name))))
            (when (and choices-present-p
                       (not (or (listp choices)
                                (symbolp choices)
                                (functionp choices))))
              (error ":CHOICES for ~s must be a list or function designator"
                     name))
            (when (and (getf plist ':convert)
                       (not (eq (getf plist ':convert) t)))
              (error ":CONVERT for ~s must be NIL or T" name))
            `(make-instance
              'command-argument
              :name                ',name
              :kind                ,(if option-p '':option '':positional)
              :value-type          ,(command--literal-form value-type)
              :required-p          ,(if required-present-p
                                        specified-required
                                        required-p)
              :repeating-p         ,repeating-p
              :short-name          ,short
              :long-name           ,long
              :documentation       ,(getf plist ':help)
              :choices             ,(if choices-present-p
                                        (command--literal-form choices)
                                        nil)
              :completion-function ,(command--literal-form
                                      (getf plist ':completion))
              :metavariable        ,metavariable
              :convert-p           ,(and (getf plist ':convert) t)
              :converter           ,(command--literal-form
                                      (getf plist ':converter))
              :keyword             ,(and option-p
                                         `',(command--parameter-keyword
                                             parameter)))))))))

(defun command--invoker-bindings (parameters parse-variable)
  "Return LET* bindings that reconstruct LAMBDA-LIST from PARSE-VARIABLE."
  (let ((bindings nil))
    (dolist (parameter parameters)
      (let ((name     (command--parameter-name parameter))
            (role     (command--parameter-role parameter))
            (initform (command--parameter-initform parameter))
            (supplied (command--parameter-supplied-name parameter)))
        (ecase role
          (:required
           (push `(,name (command--parsed-value ,parse-variable ',name))
                 bindings))
          (:optional
           (let ((present (gensym "PRESENT")))
             (push `(,present
                     (command--parsed-present-p ,parse-variable ',name))
                   bindings)
             (push `(,name
                     (if ,present
                         (command--parsed-value ,parse-variable ',name)
                         ,initform))
                   bindings)
             (when supplied
               (push `(,supplied ,present) bindings))))
          (:rest
           (push `(,name (command--parsed-values ,parse-variable ',name))
                 bindings))
          (:keyword-rest
           (push `(,name (command--parse-keyword-arguments ,parse-variable))
                 bindings))
          (:key
           (let ((present (gensym "PRESENT")))
             (push `(,present
                     (command--parsed-present-p ,parse-variable ',name))
                   bindings)
             (push `(,name
                     (if ,present
                         (command--parsed-value ,parse-variable ',name)
                         ,initform))
                   bindings)
             (when supplied
               (push `(,supplied ,present) bindings))))
          (:aux
           (push `(,name ,initform) bindings)))))
    (nreverse bindings)))

(defmacro defcommand (name (&rest lambda-list) &body body)
  "Define NAME as a Lisp function and shell command.
An optional (:ARGUMENTS SPEC...) declaration after the docstring enriches the
inferred lambda-list metadata and enables shell option parsing. Each SPEC is
(NAME :TYPE TYPE :HELP STRING ...). TYPE metadata remains advisory unless
:CONVERT T is present. :CONVERTER, dynamic :CHOICES and :COMPLETION functions
all receive (ARGUMENT CONTEXT). A converter reads the raw text from the
context's PREFIX. Existing definitions without a declaration retain raw
string argument invocation."
  (multiple-value-bind (documentation forms)
      (if (and (stringp (first body)) (rest body))
          (values (first body) (rest body))
          (values nil body))
    (let* ((declaration
             (and (consp (first forms))
                  (eq (first (first forms)) ':arguments)
                  (pop forms)))
           (parameters     (command--lambda-parameters lambda-list))
           (specifications
             (if declaration
                 (command--argument-specifications declaration parameters)
                 nil))
           (argument-forms
             (loop for parameter in parameters
                   unless (member (command--parameter-role parameter)
                                  '(:aux :keyword-rest))
                     collect
                     (command--parameter-argument-form
                      parameter
                      (or (assoc (command--parameter-name parameter)
                                 specifications :test #'eq)
                          (list (command--parameter-name parameter))))))
           (invoker-declarations
             (loop for form in forms
                   while (and (consp form) (eq (first form) 'declare))
                   collect form))
           (invoker-forms (nthcdr (length invoker-declarations) forms))
           (parse-variable (gensym "PARSE"))
           (invoker
             (when declaration
               `(lambda (arguments)
                  (let* ((,parse-variable
                           (command-arguments-parse
                            (symbol-value ',name) arguments))
                         ,@(command--invoker-bindings
                            parameters parse-variable))
                    ,@invoker-declarations
                    (block ,name
                      ,@invoker-forms))))))
      `(progn
         (defun ,name ,lambda-list
           ,@(when documentation (list documentation))
           ,@forms)
         (defparameter ,name
           (make-instance 'command
                          :name                    ',name
                          :documentation           ,documentation
                          :function                (function ,name)
                          :arguments               (list ,@argument-forms)
                          :declarative-arguments-p ,(not (null declaration))
                          :invoker                 ,invoker))
         ',name))))


;;; Declared argument parsing

(defstruct (command--parse
            (:constructor command--make-parse ()))
  "Mutable result of parsing one declared command invocation."
  (values             (make-hash-table :test #'eq))
  (present            (make-hash-table :test #'eq))
  (used-options       nil)
  (positional-index   0)
  (pending-option     nil)
  (end-of-options-p   nil)
  (keyword-arguments  nil))

(defvar *command-argument-structural-parse* nil
  "True while completion scans arguments without running value callbacks.")

(defun command--argument-failure (command control &rest arguments)
  "Signal a COMMAND-ARGUMENT-ERROR for COMMAND using CONTROL."
  (error 'command-argument-error
         :command command
         :message (apply #'format nil control arguments)))

(defun command--parsed-present-p (parse name)
  "True when PARSE contains a supplied value for argument NAME."
  (nth-value 1 (gethash name (command--parse-present parse))))

(defun command--parsed-values (parse name)
  "Return all values supplied for argument NAME in PARSE."
  (copy-list (gethash name (command--parse-values parse))))

(defun command--parsed-value (parse name)
  "Return the first supplied value for argument NAME in PARSE."
  (first (gethash name (command--parse-values parse))))

(defun command--argument-context
    (command argument words parse &key prefix buffer cursor)
  "Build callback context for ARGUMENT from an in-progress PARSE."
  (make-command-completion-context
   :command          command
   :argument         argument
   :words            (copy-list words)
   :used-options     (reverse (command--parse-used-options parse))
   :positional-index (command--parse-positional-index parse)
   :options-enabled-p (not (command--parse-end-of-options-p parse))
   :prefix           prefix
   :buffer           buffer
   :cursor           cursor))

(defun command--argument-value-string (value)
  "Render a declarative value as command-line argument text."
  (typecase value
    (string value)
    (pathname (namestring value))
    (t (prin1-to-string value))))

(defun command--choice-match (text choices)
  "Return the matching choice and true, or NIL and NIL."
  (let ((tail
          (member text choices
                  :test (lambda (needle choice)
                          (string= needle
                                   (command--argument-value-string choice))))))
    (if tail
        (values (first tail) t)
        (values nil nil))))

(defun command--read-number (text)
  "Read TEXT as one number without reader evaluation, or return NIL."
  (multiple-value-bind (value position)
      (ignore-errors
        (let ((*read-eval* nil))
          (read-from-string text)))
    (and position
         (= position (length text))
         (numberp value)
         value)))

(defun command--boolean-value (command argument text)
  "Convert explicit boolean TEXT for ARGUMENT or signal for COMMAND."
  (cond ((member text '("1" "true" "yes" "on") :test #'string-equal)
         t)
        ((member text '("0" "false" "no" "off") :test #'string-equal)
         nil)
        (t
         (command--argument-failure
          command "~a expects true or false, not ~s"
          (command-argument-name argument) text))))

(defun command--convert-argument-value
    (command argument text words parse)
  "Convert declared argument TEXT when ARGUMENT explicitly requests it."
  (let ((context
          (command--argument-context command argument words parse
                                     :prefix text)))
    (cond ((command-argument-converter argument)
           (funcall (command-argument-converter argument)
                    argument context))
          ((eq (command-argument-value-type argument) ':boolean)
           (if (eq text t)
               t
               (command--boolean-value command argument text)))
          ((not (command-argument-convert-p argument))
           text)
          ((eq (command-argument-value-type argument) ':string)
           text)
          ((eq (command-argument-value-type argument) ':integer)
           (handler-case
               (parse-integer text :junk-allowed nil)
             (error ()
               (command--argument-failure
                command "~a expects an integer, not ~s"
                (command-argument-name argument) text))))
          ((eq (command-argument-value-type argument) ':number)
           (or (command--read-number text)
               (command--argument-failure
                command "~a expects a number, not ~s"
                (command-argument-name argument) text)))
          ((member (command-argument-value-type argument)
                   '(:path :pathname :directory))
           (pathname text))
          ((eq (command-argument-value-type argument) ':package)
           (or (find-package text)
               (find-package (string-upcase text))
               (command--argument-failure
                command "~a names no package: ~s"
                (command-argument-name argument) text)))
          ((member (command-argument-value-type argument)
                   '(:environment :environment-variable))
           text)
          ((eq (command-argument-value-type argument) ':command)
           (multiple-value-bind (kind target)
               (command-resolve-fresh text)
             (if (eq kind ':unknown)
                 (command--argument-failure
                  command "~a names no command: ~s"
                  (command-argument-name argument) text)
                 target)))
          ((eq (command-argument-value-type argument) ':job)
           (unless (fboundp 'job-find)
             (command--argument-failure
              command "~a cannot resolve jobs before job control is loaded"
              (command-argument-name argument)))
           (or (funcall (symbol-function 'job-find) text)
               (command--argument-failure
                command "~a names no job: ~s"
                (command-argument-name argument) text)))
          ((eq (command-argument-value-type argument) ':choice)
           (let ((choices
                   (command-argument-choice-values argument context)))
             (multiple-value-bind (choice found-p)
                 (command--choice-match text choices)
               (if found-p
                   choice
                   (command--argument-failure
                    command "~a must be one of ~{~a~^, ~}, not ~s"
                    (command-argument-name argument)
                    (mapcar #'command--argument-value-string choices)
                    text)))))
          ((or (functionp (command-argument-value-type argument))
               (and (symbolp (command-argument-value-type argument))
                    (fboundp (command-argument-value-type argument))))
           (funcall (command-argument-value-type argument)
                    argument context))
          (t
           (command--argument-failure
            command "~a has unknown conversion type ~s"
            (command-argument-name argument)
            (command-argument-value-type argument))))))

(defun command--record-argument
    (command argument value words parse &key option-p)
  "Convert and record one ARGUMENT VALUE in PARSE."
  (let ((name (command-argument-name argument)))
    (when (and (command--parsed-present-p parse name)
               (not (command-argument-repeating-p argument)))
      (command--argument-failure
       command "~a was supplied more than once" name))
    (let ((converted
            (if *command-argument-structural-parse*
                value
                (command--convert-argument-value
                 command argument value words parse))))
      (setf (gethash name (command--parse-present parse)) t)
      (setf (gethash name (command--parse-values parse))
            (append (gethash name (command--parse-values parse))
                    (list converted)))
      (when option-p
        (push argument (command--parse-used-options parse))
        (setf (command--parse-keyword-arguments parse)
              (append (command--parse-keyword-arguments parse)
                      (list (command--argument-keyword argument)
                            converted))))
      converted)))

(defun command--option-long (command name)
  "Find COMMAND's option whose long name equals NAME."
  (find-if
   (lambda (argument)
     (and (eq (command-argument-kind argument) ':option)
          (command-argument-long-name argument)
          (string= name (command-argument-long-name argument))))
   (command-arguments command)))

(defun command--option-short (command character)
  "Find COMMAND's option whose short name is CHARACTER."
  (find-if
   (lambda (argument)
     (and (eq (command-argument-kind argument) ':option)
          (command-argument-short-name argument)
          (char= character (command-argument-short-name argument))))
   (command-arguments command)))

(defun command--positional-arguments (command)
  "Return COMMAND's positional argument metadata in declaration order."
  (remove-if-not
   (lambda (argument)
     (eq (command-argument-kind argument) ':positional))
   (command-arguments command)))

(defun command--next-positional (command parse)
  "Return the positional argument expected next by PARSE."
  (let ((index       (command--parse-positional-index parse))
        (positionals (command--positional-arguments command)))
    (or (nth index positionals)
        (let ((last (first (last positionals))))
          (and last
               (command-argument-repeating-p last)
               last)))))

(defun command--numeric-positional-token-p (command parse text)
  "True when TEXT is a negative value for the next numeric positional."
  (let ((argument (command--next-positional command parse)))
    (and argument
         (member (command-argument-value-type argument) '(:integer :number))
         (> (length text) 1)
         (char= (char text 0) #\-)
         (command--read-number text)
         t)))

(defun command--record-positional (command value words parse)
  "Record positional VALUE for COMMAND in PARSE."
  (let ((argument (command--next-positional command parse)))
    (unless argument
      (command--argument-failure command "too many positional arguments"))
    (command--record-argument command argument value words parse)
    (incf (command--parse-positional-index parse))))

(defun command--record-long-option
    (command token arguments index words parse allow-incomplete)
  "Record long option TOKEN and return the next argument INDEX."
  (let* ((equals (position #\= token :start 2))
         (name   (subseq token 2 equals))
         (option (command--option-long command name)))
    (unless option
      (command--argument-failure command "unknown option --~a" name))
    (cond ((eq (command-argument-value-type option) ':boolean)
           (command--record-argument
            command option (if equals (subseq token (1+ equals)) t)
            words parse :option-p t)
           (1+ index))
          (equals
           (command--record-argument
            command option (subseq token (1+ equals)) words parse
            :option-p t)
           (1+ index))
          ((< (1+ index) (length arguments))
           (command--record-argument
            command option (nth (1+ index) arguments) words parse
            :option-p t)
           (+ index 2))
          (allow-incomplete
           (setf (command--parse-pending-option parse) option)
           (1+ index))
          (t
           (command--argument-failure
            command "option --~a needs ~a"
            name (command-argument-metavariable option))))))

(defun command--record-short-options
    (command token arguments index words parse allow-incomplete)
  "Record short options in TOKEN and return the next argument INDEX."
  (let ((position 1)
        (next     (1+ index)))
    (loop while (< position (length token))
          do (let* ((character (char token position))
                    (option    (command--option-short command character)))
               (unless option
                 (command--argument-failure
                  command "unknown option -~c" character))
               (cond ((eq (command-argument-value-type option) ':boolean)
                      (command--record-argument
                       command option t words parse :option-p t)
                      (incf position))
                     ((< (1+ position) (length token))
                      (command--record-argument
                       command option (subseq token (1+ position)) words parse
                       :option-p t)
                      (setf position (length token)))
                     ((< next (length arguments))
                      (command--record-argument
                       command option (nth next arguments) words parse
                       :option-p t)
                      (incf next)
                      (setf position (length token)))
                     (allow-incomplete
                      (setf (command--parse-pending-option parse) option)
                      (setf position (length token)))
                     (t
                      (command--argument-failure
                       command "option -~c needs ~a"
                       character
                       (command-argument-metavariable option))))))
    next))

(defun command--validate-required-arguments (command parse)
  "Signal when PARSE omits a required argument of COMMAND."
  (dolist (argument (command-arguments command))
    (when (and (command-argument-required-p argument)
               (not (command--parsed-present-p
                     parse (command-argument-name argument))))
      (if (eq (command-argument-kind argument) ':option)
          (command--argument-failure
           command "missing required option ~a"
           (if (command-argument-long-name argument)
               (format nil "--~a" (command-argument-long-name argument))
               (format nil "-~c" (command-argument-short-name argument))))
          (command--argument-failure
           command "missing required argument ~a"
           (command-argument-metavariable argument))))))

(defun command-arguments-parse
    (command arguments &key allow-incomplete structural)
  "Parse raw ARGUMENTS according to COMMAND's declarative schema.
The returned object is intentionally opaque. ALLOW-INCOMPLETE suppresses
missing-value and required-argument failures so completion can inspect a
prefix. STRUCTURAL also skips converters and choice callbacks. Unknown and
malformed options still signal COMMAND-ARGUMENT-ERROR.
Legacy commands without an :ARGUMENTS declaration are rejected because their
execution deliberately continues to receive raw strings."
  (unless (command-declarative-arguments-p command)
    (command--argument-failure
     command "does not declare parsed command arguments"))
  (let ((parse (command--make-parse))
        (index 0)
        (*command-argument-structural-parse* structural))
    (loop while (< index (length arguments))
          for token = (nth index arguments)
          for prior = (subseq arguments 0 index)
          do (cond ((command--parse-end-of-options-p parse)
                    (command--record-positional command token prior parse)
                    (incf index))
                   ((string= token "--")
                    (setf (command--parse-end-of-options-p parse) t)
                    (incf index))
                   ((and (> (length token) 2)
                         (string= token "--" :end1 2))
                    (setf index
                          (command--record-long-option
                           command token arguments index prior parse
                           allow-incomplete)))
                   ((and (> (length token) 1)
                         (char= (char token 0) #\-)
                         (not (command--numeric-positional-token-p
                               command parse token)))
                    (setf index
                          (command--record-short-options
                           command token arguments index prior parse
                           allow-incomplete)))
                   (t
                    (command--record-positional command token prior parse)
                    (incf index))))
    (unless allow-incomplete
      (command--validate-required-arguments command parse))
    parse))

(defun command-arguments-context
    (command words &key prefix buffer cursor)
  "Return the completion callback context after complete argument WORDS.
PREFIX is the word fragment currently being completed and is not parsed.
The parser is shared with execution, including option grouping and -- rules."
  (let* ((parse
           (command-arguments-parse command words
                                    :allow-incomplete t
                                    :structural t))
         (argument
           (or (command--parse-pending-option parse)
               (command--next-positional command parse))))
    (command--argument-context
     command argument words parse
     :prefix prefix :buffer buffer :cursor cursor)))


;;; Generated help

(defun command--coerce (designator)
  "Return the COMMAND named by DESIGNATOR or signal that it is unknown."
  (let ((command
          (etypecase designator
            (command designator)
            (symbol
             (and (boundp designator)
                  (typep (symbol-value designator) 'command)
                  (symbol-value designator)))
            (string
             (multiple-value-bind (symbol found)
                 (find-symbol (string-upcase designator) *package*)
               (and found
                    (boundp symbol)
                    (typep (symbol-value symbol) 'command)
                    (symbol-value symbol)))))))
    (or command
        (error 'command-not-found-error
               :name (command-designator-name designator)))))

(defun command--usage-argument (argument)
  "Return ARGUMENT's synopsis fragment."
  (let* ((name (command-argument-metavariable argument))
         (text (if (command-argument-repeating-p argument)
                   (concatenate 'string name "...")
                   name)))
    (if (command-argument-required-p argument)
        text
        (format nil "[~a]" text))))

(defun command--option-label (argument)
  "Return ARGUMENT's short and long spellings for generated help."
  (let* ((boolean-p (eq (command-argument-value-type argument) ':boolean))
         (value     (unless boolean-p
                      (command-argument-metavariable argument)))
         (short
           (and (command-argument-short-name argument)
                (if value
                    (format nil "-~c ~a"
                            (command-argument-short-name argument) value)
                    (format nil "-~c"
                            (command-argument-short-name argument)))))
         (long
           (and (command-argument-long-name argument)
                (if value
                    (format nil "--~a ~a"
                            (command-argument-long-name argument) value)
                    (format nil "--~a"
                            (command-argument-long-name argument))))))
    (cond ((and short long)
           (format nil "~a, ~a" short long))
          (short short)
          (long long)
          (t
           (symbol-name (command-argument-name argument))))))

(defun command--argument-help (argument)
  "Return ARGUMENT's prose help, required state and static choices."
  (let ((documentation (command-argument-documentation argument))
        (choices       (command-argument-choices argument))
        (parts         nil))
    (when (and (eq (command-argument-kind argument) ':option)
               (command-argument-required-p argument))
      (push "Required." parts))
    (when documentation
      (push documentation parts))
    (when (and (listp choices) choices)
      (push (format nil "Choices: ~{~a~^, ~}."
                    (mapcar #'command--argument-value-string choices))
            parts))
    (format nil "~{~a~^ ~}" (nreverse parts))))

(defun command-help-string (designator)
  "Return generated plain-text help for a command DESIGNATOR."
  (let* ((command     (command--coerce designator))
         (arguments   (command-arguments command))
         (positionals
           (remove-if-not
            (lambda (argument)
              (eq (command-argument-kind argument) ':positional))
            arguments))
         (options
           (remove-if-not
            (lambda (argument)
              (eq (command-argument-kind argument) ':option))
            arguments)))
    (with-output-to-string (output)
      (format output "Usage: ~(~a~)~:[~; [OPTIONS]~]~{ ~a~}~%"
              (command-name command)
              (not (null options))
              (mapcar #'command--usage-argument positionals))
      (when (command-documentation command)
        (format output "~%~a~%" (command-documentation command)))
      (when positionals
        (format output "~%Arguments:~%")
        (dolist (argument positionals)
          (format output "  ~18a  ~a~%"
                  (command-argument-metavariable argument)
                  (command--argument-help argument))))
      (when (or options (command-declarative-arguments-p command))
        (format output "~%Options:~%")
        (dolist (argument options)
          (format output "  ~18a  ~a~%"
                  (command--option-label argument)
                  (command--argument-help argument)))
        (when (command-declarative-arguments-p command)
          (format output "  ~18a  Show this help.~%" "--help"))))))

(defun command-print-help (designator &optional (stream *standard-output*))
  "Print generated help for command DESIGNATOR to STREAM and return zero."
  (write-string (command-help-string designator) stream)
  (force-output stream)
  0)

(defun command--help-request-p (command arguments)
  "True when declared COMMAND receives --help as an option.
An option which takes a value consumes the following --help token. Legacy
commands do not opt into automatic help and continue to receive every raw
argument unchanged."
  (and
   (command-declarative-arguments-p command)
   (loop with index = 0
         with options-enabled-p = t
         while (< index (length arguments))
         for token = (nth index arguments)
         do (cond
              ((not options-enabled-p)
               (incf index))
              ((string= token "--")
               (setf options-enabled-p nil)
               (incf index))
              ((string= token "--help")
               (return t))
              ((and (> (length token) 2)
                    (string= token "--" :end1 2))
               (let* ((equals (position #\= token :start 2))
                      (name   (subseq token 2 equals))
                      (option (command--option-long command name)))
                 (incf index)
                 (when (and option
                            (not equals)
                            (not (eq (command-argument-value-type option)
                                     ':boolean)))
                   (incf index))))
              ((and (> (length token) 1)
                    (char= (char token 0) #\-))
               (let ((position       1)
                     (consume-next-p nil))
                 (loop while (< position (length token))
                       for option = (command--option-short
                                     command (char token position))
                       do (cond
                            ((null option)
                             (incf position))
                            ((eq (command-argument-value-type option)
                                 ':boolean)
                             (incf position))
                            (t
                             (setf consume-next-p
                                   (= (1+ position) (length token)))
                             (setf position (length token)))))
                 (incf index)
                 (when consume-next-p
                   (incf index))))
              (t
               (incf index)))
         finally (return nil))))


;;; Path utilities

(defun pathname-directory-form-p (pathname)
  "True when PATHNAME is in directory form: no name and no type.
   Dotfiles like .hidden parse with a NIL name but a non-NIL type, so
   checking the name alone misclassifies them."
  (and (null (pathname-name pathname))
       (null (pathname-type pathname))))


;;; PATH lookup

(defun path-directories ()
  "Return the directories listed in the PATH environment variable."
  (let ((path (or (getenv "PATH") "")))
    (loop with start = 0
          for split = (position #\: path :start start)
          for piece = (subseq path start split)
          when (plusp (length piece))
            collect piece
          while split
          do (setf start (1+ split)))))

(defun path--search-uncached (name)
  "Scan the PATH directories for an executable file called NAME."
  (loop for directory in (path-directories)
        for candidate = (concatenate 'string directory "/" name)
        for found = (ignore-errors (probe-file candidate))
        when (and found (not (pathname-directory-form-p found)))
          return (namestring found)))

(defun path-search (name)
  "Find the executable NAME in PATH. Returns a namestring or NIL.
   Results are cached until PATH changes or REHASH clears the cache."
  (let ((path (or (getenv "PATH") "")))
    (unless (equal path *path-cache-source*)
      (clrhash *path-cache*)
      (setf *path-cache-source* path)))
  (multiple-value-bind (cached present)
      (gethash name *path-cache*)
    (if present
        cached
        (setf (gethash name *path-cache*) (path--search-uncached name)))))


;;; Resolution

(defun path-command-names-note (name)
  "Record NAME in the completion name index after a fresh PATH hit."
  (when *path-command-names*
    (multiple-value-bind (start end)
        (structlisp:sorted-string-index-equal-range *path-command-names* name)
      (when (= start end)
        (structlisp:sorted-string-index-insert *path-command-names* name))))
  (values))

(defun command-resolve (word)
  "Resolve WORD to a runnable command.
   Returns (values :builtin command), (values :external path) or
   (values :unknown nil). Words containing a slash bypass builtins and
   PATH and are treated as direct file paths."
  (cond ((find #\/ word)
         (let ((found (ignore-errors (probe-file word))))
           (if (and found (not (pathname-directory-form-p found)))
               (values ':external (namestring found))
               (values ':unknown nil))))
        (t
         (let ((symbol (find-symbol (string-upcase word) *package*)))
           (if (and symbol
                    (boundp symbol)
                    (typep (symbol-value symbol) 'command))
               (values ':builtin (symbol-value symbol))
               (let ((path (path-search word)))
                 (if path
                     (values ':external path)
                     (values ':unknown nil))))))))

(defun word-evaluates-alone-p (word)
  "True when WORD typed alone would evaluate as Lisp rather than fail
   as an unknown command: it is a keyword or number literal, or names
   a bound variable. Uses FIND-SYMBOL so probing never interns, and
   its second value so the symbol NIL still counts as found."
  (cond ((and (> (length word) 1)
              (char= (char word 0) #\:))
         t)
        ((multiple-value-bind (symbol found)
             (find-symbol (string-upcase word) *package*)
           (and found (boundp symbol)))
         t)
        ((lisp-number-text-p word)
         (multiple-value-bind (object position)
             (ignore-errors
               (let ((*read-eval* nil))
                 (read-from-string word)))
           (and position
                (numberp object)
                (= position (length word))
                t)))
        (t
         nil)))

(defun command-resolve-fresh (word)
  "Resolve WORD like COMMAND-RESOLVE, but retry a PATH miss with a
   fresh filesystem scan. Highlighting caches a miss for every prefix
   typed, so a program installed after the cache was built would stay
   invisible until REHASH; execution paths use this instead and pay
   the rescan only when the cache says no. A fresh hit backfills the
   lookup and completion caches."
  (multiple-value-bind (kind target)
      (command-resolve word)
    (if (and (eq kind ':unknown)
             (not (find #\/ word)))
        (let ((found (path--search-uncached word)))
          (if found
              (progn
                (setf (gethash word *path-cache*) found)
                (path-command-names-note word)
                (values ':external found))
              (values ':unknown nil)))
        (values kind target))))


;;; Execution

(defun command-execute-builtin (command arguments)
  "Invoke builtin COMMAND with raw ARGUMENTS and return an exit status.
--help prints generated help without entering the command body. Commands with
declarative arguments parse them through their generated invoker; legacy
commands receive the original string list unchanged. Argument failures print
one diagnostic and return status 2."
  (handler-case
      (cond ((command--help-request-p command arguments)
             (command-print-help command))
            (t
             (let ((result
                     (if (command-declarative-arguments-p command)
                         (funcall (command--invoker command) arguments)
                         (apply (command-function command) arguments))))
               (if (integerp result) result 0))))
    (command-argument-error (condition)
      (format *error-output* "cclsh: ~a~%" condition)
      (force-output *error-output*)
      2)))

(defun command-designator-name (designator)
  "Normalize a command DESIGNATOR (symbol or string) to a name string."
  (etypecase designator
    (symbol (string-downcase (symbol-name designator)))
    (string designator)))
