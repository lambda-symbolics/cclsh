;;;; -- Regression checks --

(require :asdf)

(asdf:load-asd (truename "cclsh.asd"))
(asdf:load-system "cclsh")

(defvar *check-failures* nil
  "Descriptions of failed regression checks.")

(defun check-equal (name expected actual)
  "Record a failure under NAME unless EXPECTED and ACTUAL are EQUAL."
  (unless (equal expected actual)
    (push (format nil "~a: expected ~s, got ~s" name expected actual)
          *check-failures*)))

(defun check-history (&rest entries)
  "Return a history vector containing ENTRIES."
  (make-array (length entries)
              :adjustable       t
              :fill-pointer     (length entries)
              :initial-contents entries))

(defun check-write-utf8-file (path contents)
  "Write CONTENTS to PATH as UTF-8 and return PATH."
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create
                          :external-format ':utf-8)
    (write-string contents stream))
  path)

(defun check-edit-fixture (value)
  "Source-note fixture for the EDIT command checks."
  (+ value 1))

(defun check-set-locale (locale)
  "Set Linux LC_ALL to LOCALE, or query it when LOCALE is NIL."
  (let ((pointer
          (if locale
              (ccl::with-utf-8-cstr (encoded locale)
                (ccl:external-call "setlocale"
                                   :int 6 :address encoded :address))
              (ccl:external-call "setlocale"
                                 :int 6 :address (ccl:%null-ptr) :address))))
    (unless (ccl:%null-ptr-p pointer)
      (ccl::%get-utf-8-cstring pointer))))

(defun check-signal-blocked-p (signal)
  "True when SIGNAL is blocked in the current native thread."
  (ccl:%stack-block ((signals cclsh::+terminal-sigset-size+))
    (unless
        (zerop
         (ccl:external-call "pthread_sigmask"
                            :int cclsh::+terminal-sig-block+
                            :address (ccl:%null-ptr)
                            :address signals
                            :int))
      (error "cannot inspect the signal mask"))
    (= 1 (ccl:external-call "sigismember"
                            :address signals
                            :int signal
                            :int))))

(defun check-path-mode (path)
  "Return PATH permissions as three or four octal digits."
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (uiop:run-program (list "stat" "-c" "%a" path)
                                 :output ':string)))

(defvar *check-stage-arguments* nil
  "Last argument vector received by CHECK-STAGE-ARGUMENTS.")

(cclsh:defcommand check-stage-arguments (&rest arguments)
  "Record ARGUMENTS for stage-expansion regression checks."
  (setf *check-stage-arguments* (copy-list arguments))
  23)

(cclsh:defcommand check-stage-emit (&rest arguments)
  "Print ARGUMENTS for pipeline stage-expansion regression checks."
  (format t "~s~%" arguments)
  41)

(cclsh:defcommand check-run-program (program &rest arguments)
  "Run external PROGRAM for dynamic pipeline-context checks."
  (apply #'cclsh:run program arguments))

(cclsh:defcommand check-run-order ()
  "Mix Lisp and child output without manually flushing either stream."
  (write-string "before|")
  (cclsh:run "/usr/bin/printf" "child|")
  (write-string "after|")
  0)

(defvar *check-declared-command-call* nil
  "Arguments observed by CHECK-DECLARED-COMMAND.")

(defvar *check-declared-choice-context* nil
  "Last completion context received while resolving dynamic choices.")

(defun check-declared-choices (argument context)
  "Return dynamic format choices and retain callback inputs."
  (setf *check-declared-choice-context* (list argument context))
  '("text" "json"))

(defun check-declared-completion (argument context)
  "Return a completion fixture for declarative metadata checks."
  (declare (ignore argument context))
  (values '("fixture") '("test completion")))

(defun check-declared-converter (argument context)
  "Convert context text while proving the custom callback contract."
  (list (cclsh:command-completion-context-prefix context)
        (cclsh:command-argument-name argument)
        (cclsh:command-name
         (cclsh:command-completion-context-command context))))

(cclsh:defcommand check-declared-command
    (source &optional (mode "safe" mode-p)
            &key (count 1 count-p) verbose dry-run format tag)
  "Exercise declarative command parsing."
  (:arguments
   (source :type :directory
           :help "Source directory."
           :completion #'check-declared-completion)
   (mode :choices ("safe" "fast") :convert t :help "Operating mode.")
   (count :type :integer :convert t :short #\n :help "Repeat count.")
   (verbose :type :boolean :short #\v :help "Enable verbose output.")
   (dry-run :type :boolean :short #\d :help "Do not make changes.")
   (format :choices check-declared-choices :convert t :short #\f
           :help "Output format.")
   (tag :converter #'check-declared-converter :help "Attach a tag."))
  (setf *check-declared-command-call*
        (list source mode mode-p count count-p verbose dry-run format tag))
  37)

(defvar *check-declared-rest-call* nil
  "Arguments observed by CHECK-DECLARED-REST.")

(cclsh:defcommand check-declared-rest (head &rest tail)
  "Exercise required and repeating positional parsing."
  (:arguments
   (head :type :integer :convert t :help "First number.")
   (tail :type :number :convert t :help "Remaining numbers."))
  (setf *check-declared-rest-call* (cons head tail))
  19)

(defvar *check-semantic-command-call* nil
  "Converted semantic values observed by CHECK-SEMANTIC-COMMAND.")

(cclsh:defcommand check-semantic-command (path package-name program)
  "Exercise path, package and command semantic conversion."
  (:arguments
   (path :type :path :convert t)
   (package-name :type :package :convert t)
   (program :type :command :convert t))
  (setf *check-semantic-command-call* (list path package-name program))
  29)

(defvar *check-runtime-semantic-call* nil
  "Converted environment name and job observed by CHECK-RUNTIME-SEMANTIC.")

(cclsh:defcommand check-runtime-semantic (variable job)
  "Exercise environment-name and job semantic conversion."
  (:arguments
   (variable :type :environment-variable :convert t)
   (job :type :job :convert t))
  (setf *check-runtime-semantic-call* (list variable job))
  30)

(defvar *check-declared-keyword-call* nil
  "Arguments observed by CHECK-DECLARED-KEYWORD.")

(cclsh:defcommand check-declared-keyword
    (&key ((:output destination) "stdout" destination-p) required)
  "Exercise an explicit keyword name and a required short-only option."
  (:arguments
   (destination :type :string :help "Output destination.")
   (required :type :string :required t :short #\r :long nil
             :help "Required marker."))
  (setf *check-declared-keyword-call*
        (list destination destination-p required))
  31)


;;;; -- Declarative commands --

(let ((arguments (cclsh:command-arguments check-declared-command)))
  (check-equal "declarative metadata is marked as declared"
               t
               (cclsh:command-declarative-arguments-p
                check-declared-command))
  (check-equal "declarative metadata preserves lambda-list order"
               '(source mode count verbose dry-run format tag)
               (mapcar #'cclsh:command-argument-name arguments))
  (check-equal "required positional metadata"
               '(:positional t nil :directory)
               (let ((argument (first arguments)))
                 (list (cclsh:command-argument-kind argument)
                       (cclsh:command-argument-required-p argument)
                       (cclsh:command-argument-repeating-p argument)
                       (cclsh:command-argument-value-type argument))))
  (check-equal "optional positional metadata"
               '(:positional nil :choice)
               (let ((argument (second arguments)))
                 (list (cclsh:command-argument-kind argument)
                       (cclsh:command-argument-required-p argument)
                       (cclsh:command-argument-value-type argument))))
  (check-equal "value option metadata"
               '(:option #\n "count" :integer t)
               (let ((argument (third arguments)))
                 (list (cclsh:command-argument-kind argument)
                       (cclsh:command-argument-short-name argument)
                       (cclsh:command-argument-long-name argument)
                       (cclsh:command-argument-value-type argument)
                       (cclsh:command-argument-convert-p argument))))
  (check-equal "custom completion metadata remains callable"
               t
               (functionp
                (cclsh:command-argument-completion-function
                 (first arguments)))))

(dolist (case
          '(("required lambda parameters cannot be weakened"
             (cclsh:defcommand check-invalid-optional (value)
               (:arguments (value :required nil))
               value))
            ("metavariables must be strings"
             (cclsh:defcommand check-invalid-metavariable (value)
               (:arguments (value :metavariable 12))
               value))
            ("argument properties cannot be repeated"
             (cclsh:defcommand check-duplicate-property (value)
               (:arguments (value :type :string :type :integer))
               value))
            ("completion callbacks must be function designators"
             (cclsh:defcommand check-invalid-completion (value)
               (:arguments (value :completion 12))
               value))
            ("converter callbacks must be function designators"
             (cclsh:defcommand check-invalid-converter (value)
               (:arguments (value :converter "not a function"))
               value))
            ("choices must be a list or function designator"
             (cclsh:defcommand check-invalid-choices (value)
               (:arguments (value :choices "not choices"))
               value))
            ("long options cannot be empty"
             (cclsh:defcommand check-invalid-long-empty (&key value)
               (:arguments (value :long ""))
               value))
            ("long options cannot begin with a dash"
             (cclsh:defcommand check-invalid-long-dash (&key value)
               (:arguments (value :long "-value"))
               value))
            ("long options cannot contain whitespace"
             (cclsh:defcommand check-invalid-long-space (&key value)
               (:arguments (value :long "bad value"))
               value))
            ("long options cannot contain equals"
             (cclsh:defcommand check-invalid-long-equals (&key value)
               (:arguments (value :long "bad=value"))
               value))
            ("short options cannot be a dash"
             (cclsh:defcommand check-invalid-short-dash (&key value)
               (:arguments (value :short #\-))
               value))
            ("short options cannot be whitespace"
             (cclsh:defcommand check-invalid-short-space (&key value)
               (:arguments (value :short #\Space))
               value))))
  (destructuring-bind (name definition) case
    (check-equal
     name t
     (handler-case
         (progn
           (eval definition)
           nil)
       (error ()
         t)))))

(check-equal "legacy command metadata remains advisory"
             nil
             (cclsh:command-declarative-arguments-p check-stage-arguments))
(setf *check-stage-arguments* nil)
(check-equal "legacy command invocation retains raw option-like strings"
             23
             (cclsh::command-execute-builtin
              check-stage-arguments '("--unknown" "-1")))
(check-equal "legacy command receives unparsed arguments"
             '("--unknown" "-1")
             *check-stage-arguments*)
(setf *check-stage-arguments* nil)
(check-equal "legacy command receives a raw --help argument"
             23
             (cclsh::command-execute-builtin
              check-stage-arguments '("--help" "tail")))
(check-equal "legacy --help invocation enters the command body"
             '("--help" "tail")
             *check-stage-arguments*)
(check-equal "legacy generated help does not advertise automatic help"
             nil
             (search "--help"
                     (cclsh:command-help-string check-stage-arguments)))

(setf *check-declared-choice-context* nil)
(check-equal "long, attached, grouped and converted options execute"
             37
             (cclsh::command-execute-builtin
              check-declared-command
              '("src" "--count=3" "-vd" "-fjson" "--tag" "release")))
(check-equal "declared invocation reconstructs defaults and supplied flags"
             '("src" "safe" nil 3 t t t "json"
               ("release" tag check-declared-command))
             *check-declared-command-call*)
(check-equal "dynamic choices receive argument and stable context"
             '(format check-declared-command "json")
             (let ((argument (first *check-declared-choice-context*))
                   (context  (second *check-declared-choice-context*)))
               (list (cclsh:command-argument-name argument)
                     (cclsh:command-name
                      (cclsh:command-completion-context-command context))
                     (cclsh:command-completion-context-prefix context))))

(let ((output (make-string-output-stream)))
  (let ((*standard-output* output))
    (check-equal "--help consumed as an option value reaches the body"
                 37
                 (cclsh::command-execute-builtin
                  check-declared-command '("src" "--tag" "--help"))))
  (check-equal "option-value --help does not render generated help"
               ""
               (get-output-stream-string output)))
(check-equal "custom converter receives --help as its raw option value"
             '("src" "safe" nil 1 nil nil nil nil
               ("--help" tag check-declared-command))
             *check-declared-command-call*)

(check-equal "spaced short option value executes"
             37
             (cclsh::command-execute-builtin
              check-declared-command '("src" "fast" "-n" "4")))
(check-equal "spaced option value and explicit false boolean convert"
             37
             (cclsh::command-execute-builtin
              check-declared-command
              '("src" "--verbose=false" "--format" "text")))
(check-equal "explicit false boolean reaches the command body"
             '("src" "safe" nil 1 nil nil nil "text" nil)
             *check-declared-command-call*)

(check-equal "declared command remains directly callable from Lisp"
             37
             (check-declared-command "direct" "fast"
                                     :count 7 :verbose t))
(check-equal "direct Lisp call preserves ordinary lambda-list semantics"
             '("direct" "fast" t 7 t t nil nil nil)
             *check-declared-command-call*)

(check-equal "explicit Lisp keyword name becomes the default long option"
             31
             (cclsh::command-execute-builtin
              check-declared-keyword '("--output" "file" "-r" "yes")))
(check-equal "declared invoker reconstructs explicit keyword bindings"
             '("file" t "yes")
             *check-declared-keyword-call*)
(check-equal "explicit keyword command remains directly callable"
             31
             (check-declared-keyword :output "lisp" :required "yes"))
(check-equal "direct explicit keyword call keeps its supplied flag"
             '("lisp" t "yes")
             *check-declared-keyword-call*)

(let ((error-output (make-string-output-stream)))
  (let ((*error-output* error-output))
    (check-equal "required short-only option fails when absent"
                 2
                 (cclsh::command-execute-builtin
                  check-declared-keyword nil)))
  (check-equal "required short-only diagnostic names its usable spelling"
               t
               (not (null (search "-r"
                                  (get-output-stream-string error-output))))))

(check-equal "generated help identifies required options"
             t
             (not (null (search "Required."
                                (cclsh:command-help-string
                                 check-declared-keyword)))))

(check-equal "required and repeating positionals convert"
             19
             (cclsh::command-execute-builtin
              check-declared-rest '("-1" "2.5" "3")))
(check-equal "negative numeric positional is not parsed as an option"
             '(-1 2.5 3)
             *check-declared-rest-call*)
(check-equal "option terminator permits option-shaped positionals"
             19
             (cclsh::command-execute-builtin
              check-declared-rest '("1" "--" "-2")))
(check-equal "option terminator values reach repeating positional"
             '(1 -2)
             *check-declared-rest-call*)

(let ((*package* (find-package '#:cclsh-user)))
  (check-equal "path, package and command semantic values convert"
               29
               (cclsh::command-execute-builtin
                check-semantic-command '("relative/path" "cl" "help"))))
(check-equal "path semantic conversion returns a pathname"
             (pathname "relative/path")
             (first *check-semantic-command-call*))
(check-equal "package semantic conversion resolves a package"
             (find-package '#:cl)
             (second *check-semantic-command-call*))
(check-equal "command semantic conversion resolves a command object"
             cclsh:help
             (third *check-semantic-command-call*))

(let* ((job (cclsh::job-make :command "check semantic job"))
       (cclsh::*jobs* (list job)))
  (setf (cclsh::job-id job) 71)
  (check-equal "environment and job semantic values convert"
               30
               (cclsh::command-execute-builtin
                check-runtime-semantic '("PATH" "%71")))
  (check-equal "environment semantic conversion keeps the variable name"
               "PATH"
               (first *check-runtime-semantic-call*))
  (check-equal "job semantic conversion resolves a live job object"
               job
               (second *check-runtime-semantic-call*)))

(let ((*error-output* (make-broadcast-stream))
      (*package* (find-package '#:cclsh-user)))
  (check-equal "unknown package conversion fails"
               2
               (cclsh::command-execute-builtin
                check-semantic-command
                '("path" "no-such-check-package" "help")))
  (check-equal "unknown command conversion fails"
               2
               (cclsh::command-execute-builtin
                check-semantic-command
                '("path" "cl" "no-such-check-command")))
  (let ((cclsh::*jobs* nil))
    (check-equal "unknown job conversion fails"
                 2
                 (cclsh::command-execute-builtin
                  check-runtime-semantic '("PATH" "%9999")))))

(dolist (case
          '(("missing required positional" nil)
            ("unknown long option" ("src" "--wat"))
            ("unknown short option" ("src" "-x"))
            ("missing option value" ("src" "--count"))
            ("duplicate option" ("src" "-v" "--verbose"))
            ("invalid integer" ("src" "--count" "many"))
            ("invalid static choice" ("src" "unsafe"))
            ("invalid dynamic choice" ("src" "--format" "xml"))))
  (destructuring-bind (name arguments) case
    (let ((*error-output* (make-broadcast-stream)))
      (check-equal name 2
                   (cclsh::command-execute-builtin
                    check-declared-command arguments)))))

(let ((*check-declared-command-call* ':untouched)
      (output (make-string-output-stream)))
  (let ((*standard-output* output))
    (check-equal "--help returns success" 0
                 (cclsh::command-execute-builtin
                  check-declared-command '("--help")))
    (let ((help (get-output-stream-string output)))
      (check-equal "generated help has usage"
                   t
                   (not (null (search "Usage: check-declared-command" help))))
      (check-equal "generated help has option spelling"
                   t
                   (not (null (search "-n COUNT, --count COUNT" help))))
      (check-equal "generated help has argument prose"
                   t
                   (not (null (search "Source directory." help))))))
  (check-equal "--help does not enter the command body"
               ':untouched
               *check-declared-command-call*))

(let ((output (make-string-output-stream)))
  (let ((*standard-output* output))
    (check-equal "help COMMAND returns success" 0
                 (cclsh:help "check-declared-command")))
  (check-equal "help COMMAND renders generated help"
               t
               (not (null
                     (search "Usage: check-declared-command"
                             (get-output-stream-string output))))))

(let ((output (make-string-output-stream)))
  (let ((*standard-output* output))
    (check-equal "manual section wins over command help" 0
                 (cclsh:help "jobs")))
  (check-equal "manual collision does not render command usage"
               nil
               (search "Usage: jobs" (get-output-stream-string output))))

(let ((*error-output* (make-broadcast-stream)))
  (check-equal "unknown help subject remains status one"
               1
               (cclsh:help "definitely-not-a-help-subject")))


;;;; -- Clinedi boundary --

(check-equal "Clinedi supplies ANSI presentation"
             (find-symbol "ANSI-STRIP" '#:clinedi)
             'cclsh::ansi-strip)
(check-equal "Clinedi supplies Unicode cell geometry"
             2
             (clinedi:text-cell-width "猫"))
(check-equal "cl-colorist supplies Clinedi ANSI parsing"
             "red"
             (cl-colorist:strip-ansi
              (format nil "~c[31mred~c[0m"
                      (code-char 27)
                      (code-char 27))))
(check-equal "CCLSH owns a configurable Clinedi keymap"
             t
             (typep cclsh:*line-editor-keymap* 'clinedi:keymap))

(check-equal "library load leaves terminal signal policy unclaimed"
             nil
             cclsh::*terminal-control-signals-active*)

(let ((before (check-signal-blocked-p cclsh::+sigttou+))
      (entered nil)
      (unwound nil))
  (handler-case
      (cclsh::terminal--call-with-sigttou-safe
       (cclsh::terminal-own-process-group)
       (lambda ()
         (setf entered t)
         (error "injected terminal operation failure")))
    (error ()
      (setf unwound t)))
  (check-equal "library terminal operation runs protected body" t entered)
  (check-equal "library terminal operation propagates failure" t unwound)
  (check-equal "library terminal failure restores signal mask"
               before
               (check-signal-blocked-p cclsh::+sigttou+)))


;;;; -- Prompt customization --

(let* ((*package* (find-package '#:cclsh-user))
       (prefix   (format nil "~a@~a (CCLSH-USER) "
                         (cclsh::prompt--username)
                         (cclsh::prompt--hostname)))
       (prompt   (cclsh::ansi-strip (cclsh:prompt-default :status 0))))
  (check-equal "default prompt begins with identity and package"
               0
               (search prefix prompt)))

(let ((old-package-environment (cclsh:getenv "CCLSH_PACKAGE"))
      (received                nil)
      (custom-prompt           (format nil "custom~%> ")))
  (unwind-protect
      (let* ((*package* (find-package '#:cclsh-user))
             (expected-job-count (cclsh::jobs-count))
             (cclsh:*last-status* 73)
             (cclsh:*prompt-function*
               (lambda (&key status duration-milliseconds columns job-count
                        &allow-other-keys)
                 (setf received
                       (list status duration-milliseconds columns job-count
                             cclsh:*last-status*
                             (cclsh:getenv "CCLSH_PACKAGE")))
                 (setf cclsh:*last-status* 99)
                 custom-prompt)))
        (check-equal "custom prompt result is used verbatim"
                     custom-prompt
                     (cclsh::prompt-render 7 125 91))
        (check-equal "custom prompt receives shell snapshots"
                     (list 7 125 91 expected-job-count 7 "CCLSH-USER")
                     received)
        (check-equal "custom prompt preserves the last command status"
                     73
                     cclsh:*last-status*))
    (if old-package-environment
        (cclsh:setenv "CCLSH_PACKAGE" old-package-environment)
        (cclsh::unsetenv "CCLSH_PACKAGE"))))

(let* ((identity (format nil "~a@~a"
                         (cclsh::prompt--username)
                         (cclsh::prompt--hostname)))
       (cases
         (list (list "NIL custom prompt selects the default"
                     (lambda (&key &allow-other-keys)
                       nil)
                     nil)
               (list "failing custom prompt selects the default"
                     (lambda (&key &allow-other-keys)
                       (error "injected prompt failure"))
                     "renderer failed")
               (list "non-string custom prompt selects the default"
                     (lambda (&key &allow-other-keys)
                       42)
                     "instead of a string or NIL"))))
  (dolist (case cases)
    (destructuring-bind (name renderer expected-error) case
      (let ((errors (make-string-output-stream))
            (cclsh:*prompt-function* renderer))
        (let ((*error-output* errors))
          (check-equal name
                       0
                       (search identity
                               (cclsh::ansi-strip
                                (cclsh::prompt-render 0 0 80)))))
          (let ((reported (get-output-stream-string errors)))
            (check-equal (format nil "~a error report" name)
                         (not (null expected-error))
                         (and expected-error
                              (not (null (search expected-error reported))))))))))

;;;; -- Command line arguments --

(let ((pathname (pathname "deploy.sh.lisp")))
  (check-equal "canonical script suffix remains a Lisp pathname"
               '("deploy.sh" "lisp")
               (list (pathname-name pathname) (pathname-type pathname))))

(dolist (case
          '(("plain command mode" ("-c" "echo plain")
             (:command "echo plain" nil nil))
            ("login command group" ("-lc" "echo login")
             (:command "echo login" t nil))
            ("reverse login command group" ("-cl" "echo reverse")
             (:command "echo reverse" t nil))
            ("interactive login command group" ("-ilc" "echo both")
             (:command "echo both" t nil))
            ("interactive command group" ("-ic" "echo configured")
             (:command "echo configured" t nil))
            ("separate login and command flags" ("-l" "-c" "echo split")
             (:command "echo split" t nil))
            ("unknown letters mixed with command flags"
             ("-xlc" "echo tolerant")
             (:command "echo tolerant" t nil))
            ("unknown letters mixed with plain command mode"
             ("-xc" "echo safe")
             (:command "echo safe" nil nil))
            ("command operand beginning with dash" ("-c" "-l")
             (:command "-l" nil nil))
            ("missing command" ("-c")
             (:missing-command nil nil nil))
            ("configured missing command" ("-l" "-c")
             (:missing-command nil t nil))
            ("manual overview" ("help")
             (:manual nil nil nil))
            ("manual section" ("help" "scripting")
             (:manual ("scripting") nil nil))
            ("manual preserves every section argument"
             ("help" "editing" "--version" "-c")
             (:manual ("editing" "--version" "-c") nil nil))
            ("configuration flag before manual"
             ("-l" "help" "jobs")
             (:manual ("jobs") t nil))
            ("unknown short flags" ("-xyz")
             (:main nil nil nil))
            ("configuration flags without command" ("-lix")
             (:main nil t nil))
            ("long option is not a short group" ("--lc" "script.sh.lisp")
             (:script "script.sh.lisp" nil nil))
            ("uppercase C remains unknown" ("-C" "script.sh.lisp")
             (:script "script.sh.lisp" nil nil))
            ("script stops option parsing"
             ("script.sh.lisp" "-lc" "echo not-executed")
             (:script "script.sh.lisp" nil
              ("-lc" "echo not-executed")))
            ("double dash permits a dash-prefixed script"
             ("--" "-script.sh.lisp" "--help")
             (:script "-script.sh.lisp" nil ("--help")))
            ("double dash permits a script named help"
             ("--" "help" "scripting")
             (:script "help" nil ("scripting")))
            ("explicit path permits a script named help"
             ("./help" "scripting")
             (:script "./help" nil ("scripting")))
            ("double dash without a script starts normally" ("--")
             (:main nil nil nil))))
  (destructuring-bind (name arguments expected) case
    (check-equal name expected
                 (multiple-value-list
                  (cclsh::shell--argument-plan arguments)))))

(check-equal "absolute invocation path preserves an installed symlink"
             "/usr/local/bin/cclsh"
             (cclsh::shell--invocation-path
              :arguments '("/usr/local/bin/cclsh" "-c" "exit 0")
              :executable-path "/usr/local/bin/cclsh"))

(check-equal "relative invocation path defers to the executable fallback"
             nil
             (cclsh::shell--invocation-path
              :arguments '("cclsh" "-c" "exit 0")
              :executable-path "/usr/local/bin/cclsh"))

(check-equal "login argv uses the account's stable shell path"
             "/usr/local/bin/cclsh"
             (cclsh::shell--invocation-path
              :arguments '("-cclsh")
              :environment-shell "/usr/local/bin/cclsh"
              :executable-path "/usr/local/bin/cclsh"))

(check-equal "login argv rejects a stale inherited shell"
             nil
             (cclsh::shell--invocation-path
              :arguments '("-cclsh")
              :environment-shell "/bin/sh"
              :executable-path "/usr/local/bin/cclsh"))


;;;; -- Multiline Lisp --

(dolist (line (list "; comment"
                    ";;; comment"
                    (format nil " ~c ; indented comment" #\Tab)))
  (check-equal (format nil "top-level comment classification for ~s" line)
               t
               (cclsh::line-comment-p line)))

(dolist (line '("" "   " "echo ; literal" "(list ; Lisp comment"
                "\\; escaped argument" "';' quoted argument"))
  (check-equal (format nil "non-comment classification for ~s" line)
               nil
               (cclsh::line-comment-p line)))

(let ((line (format nil "  ; unmatched ~c and ( with trailing ~c"
                    #\" #\\)))
  (check-equal "comment syntax never requests continuation"
               nil
               (cclsh::input-line-open-p line))
  (check-equal "comment highlighting preserves text"
               line
               (cclsh::ansi-strip (cclsh::highlight-line line)))
  (check-equal "comment highlighting is uniformly dim"
               (cclsh::ansi-colorize line ':bright-black)
               (cclsh::highlight-line line))
  (check-equal "comments offer no completions"
               (list (length line) nil nil)
               (multiple-value-list
                (cclsh::complete-line line (length line)))))

(check-equal "inline semicolons remain command arguments"
             '("echo" ";" "literal")
             (cclsh::command-line-words "echo ; literal"))

(let ((lisp-line (format nil "(progn~% ; ignored )~% 42)"))
      (command   (format nil "echo (progn ; ignored )~% 42)")))
  (check-equal "complete multiline Lisp comment"
               nil
               (cclsh::input-line-open-p lisp-line))
  (check-equal "complete multiline substitution comment"
               nil
               (cclsh::input-line-open-p command))
  (check-equal "multiline highlighting preserves text"
               lisp-line
               (cclsh::ansi-strip (cclsh::highlight-line lisp-line))))

(let ((complete
        '("(list #| ignored ) #| nested ( |# |# 1)"
          "(list |symbol with ) ; and hash text| 1)"
          "(list |escaped \\| and )| 1)"
          "(list #\\) 1)"
          "(list foo\\) 1)"
          "echo (list #| ignored ) |# #\\) |symbol )| foo\\))"))
      (open
        '("(list #| unfinished"
          "(list |unfinished"
          "echo (list #| unfinished"
          "echo (list |unfinished")))
  (dolist (line complete)
    (check-equal (format nil "complete reader syntax in ~s" line)
                 nil
                 (cclsh::input-line-open-p line))
    (check-equal (format nil "highlight preserves reader syntax in ~s" line)
                 line
                 (cclsh::ansi-strip (cclsh::highlight-line line))))
  (dolist (line open)
    (check-equal (format nil "unfinished reader syntax in ~s" line)
                 t
                 (not (null (cclsh::input-line-open-p line))))))


;;;; -- Completion safety --

(let ((*package* (find-package '#:cclsh-user)))
  (dolist (name '("odd name" "odd(name)" "odd&name" "odd\"name"
                  "odd'name" "odd\\name" "odd$name" "odd*name"
                  "odd?name" "~odd"))
    (let ((escaped (cclsh::completion--escape name)))
      (check-equal (format nil "completed command round-trip for ~s" name)
                   (list name)
                   (cclsh::command-line-words escaped))))
  (let* ((name    (format nil "bell~c esc~c newline~% c1~c"
                          (code-char 7) (code-char 27) (code-char 133)))
         (escaped (cclsh::completion--escape name))
         (display (cclsh::completion--display name)))
    (check-equal "control-bearing completion round-trip"
                 (list name)
                 (cclsh::command-line-words escaped))
    (check-equal "accepted completion has no terminal controls"
                 nil
                 (find-if #'cclsh::completion--terminal-control-p escaped))
    (check-equal "completion display has no terminal controls"
                 nil
                 (find-if #'cclsh::completion--terminal-control-p display)))
  (check-equal "common prefix never leaves a dangling escape"
               "foo"
               (cclsh::completion--common-prefix '("foo\\(" "foo\\)")))
  (check-equal "common prefix never inserts a partial control expression"
               ""
               (cclsh::completion--common-prefix
                (list (cclsh::completion--escape (format nil "a~%x"))
                      (cclsh::completion--escape
                       (format nil "a~cy" (code-char 27))))))
  (check-equal "unique non-directory completion adds a space"
               "printf "
               (cclsh::line-editor--accept-completion "printf"))
  (check-equal "unique directory completion remains open"
               "source/"
               (cclsh::line-editor--accept-completion "source/")))


;;;; -- Lisp glob arguments --

(let* ((root
         (format nil "/tmp/cclsh-check-glob-~d/"
                 (ccl:external-call "getpid" :int)))
       (single-path      (concatenate 'string root "only.single"))
       (alpha-path       (concatenate 'string root "alpha.multi"))
       (space-path       (concatenate 'string root "beta space.multi"))
       (single-pattern   (concatenate 'string root "*.single"))
       (multiple-pattern (concatenate 'string root "*.multi"))
       (missing-pattern  (concatenate 'string root "*.absent"))
       (old-home         (cclsh:getenv "HOME"))
       (variable         "CCLSH_CHECK_GLOB_ROOT")
       (old-variable     (cclsh:getenv variable)))
  (flet ((restore-environment (name value)
           (if value
               (cclsh:setenv name value)
               (cclsh::unsetenv name))))
    (unwind-protect
        (progn
          (check-write-utf8-file single-path "single")
          (check-write-utf8-file alpha-path "alpha")
          (check-write-utf8-file space-path "space")

          (multiple-value-bind (symbol status)
              (find-symbol "GLOB" '#:cclsh)
            (check-equal "glob is exported" ':external status)
            (check-equal "glob is callable"
                         t
                         (and symbol (fboundp symbol) t))
            (check-equal "glob is visible in cclsh-user"
                         symbol
                         (find-symbol "GLOB" '#:cclsh-user)))

          (check-equal "one-match glob returns a proper list"
                       (list single-path)
                       (cclsh:glob single-pattern))
          (check-equal "multiple glob matches are sorted"
                       (list alpha-path space-path)
                       (cclsh:glob multiple-pattern))
          (check-equal "multiple glob patterns retain their positions"
                       (list single-path alpha-path space-path)
                       (cclsh:glob single-pattern multiple-pattern))
          (check-equal "unmatched glob remains literal"
                       (list missing-pattern)
                       (cclsh:glob missing-pattern))
          (check-equal "glob without patterns returns NIL"
                       nil
                       (cclsh:glob))
          (check-equal "glob rejects a non-string pattern"
                       t
                       (handler-case
                           (progn (cclsh:glob 42) nil)
                         (type-error () t)))

          (cclsh:setenv "HOME" (string-right-trim "/" root))
          (check-equal "glob expands a leading tilde"
                       (list single-path)
                       (cclsh:glob "~/*.single"))
          (cclsh:setenv variable (string-right-trim "/" root))
          (check-equal "glob expands an environment variable"
                       (list single-path)
                       (cclsh:glob
                        (format nil "$~a/*.single" variable)))

          (check-equal "cmd glob returns the command status"
                       23
                       (cclsh:cmd check-stage-arguments
                                  "before"
                                  (cclsh:glob multiple-pattern)
                                  "after"))
          (check-equal "cmd glob splices matches in place"
                       (list "before" alpha-path space-path "after")
                       *check-stage-arguments*)
          (check-equal "cmd glob records the command status"
                       23
                       cclsh:*last-status*)

          (let ((matches (cclsh:glob multiple-pattern)))
            (cclsh:cmd check-stage-arguments matches)
            (check-equal "stored glob list remains spliceable"
                         (list alpha-path space-path)
                         *check-stage-arguments*))
          (cclsh:cmd check-stage-arguments '("left" 2))
          (check-equal "proper list stage argument splices one level"
                       (list "left" "2")
                       *check-stage-arguments*)
          (cclsh:cmd check-stage-arguments '((left right) "tail"))
          (check-equal "nested stage list does not flatten recursively"
                       (list "(LEFT RIGHT)" "tail")
                       *check-stage-arguments*)
          (cclsh:cmd check-stage-arguments nil "after")
          (check-equal "NIL stage argument contributes no words"
                       (list "after")
                       *check-stage-arguments*)
          (cclsh:cmd check-stage-arguments "*.multi")
          (check-equal "ordinary string stage argument remains literal"
                       (list "*.multi")
                       *check-stage-arguments*)

          (multiple-value-bind (text status)
              (cclsh:capture
                (check-stage-emit "before"
                                  (cclsh:glob multiple-pattern)
                                  "after"))
            (check-equal "pipeline glob splices matches"
                         (format nil "~s"
                                 (list "before" alpha-path
                                       space-path "after"))
                         text)
            (check-equal "pipeline glob keeps stage status" 41 status)))

          (check-equal "single glob may supply a redirect path"
                       41
                       (cclsh:pipe
                         (check-stage-emit "redirected")
                         (to (cclsh:glob single-pattern))))
          (check-equal "single glob redirect receives stage output"
                       (format nil "~s~%" (list "redirected"))
                       (uiop:read-file-string single-path))

          (setf *check-stage-arguments* '(:not-started))
          (check-equal "multi-match redirect fails arity validation"
                       t
                       (handler-case
                           (progn
                             (cclsh:pipe
                               (check-stage-arguments "started")
                               (to (cclsh:glob multiple-pattern)))
                             nil)
                         (cclsh::pipeline-syntax-error () t)))
          (check-equal "invalid glob redirect starts no command"
                       '(:not-started)
                       *check-stage-arguments*)
      (restore-environment "HOME" old-home)
      (restore-environment variable old-variable)
      (ignore-errors
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist ':ignore)))))


;;;; -- Pipeline-aware run --

(multiple-value-bind (text status)
    (cclsh:capture
      ("/usr/bin/printf" "nested-input")
      (check-run-program "/usr/bin/cat"))
  (check-equal "run in builtin inherits pipeline input and output"
               "nested-input"
               text)
  (check-equal "run in builtin retains successful pipeline status"
               0 status))

(multiple-value-bind (text status)
    (cclsh:capture
      (check-run-program "/bin/sh" "-c" "printf nested-error >&2")
      (merge-error))
  (check-equal "run in builtin inherits merged standard error"
               "nested-error"
               text)
  (check-equal "merged nested run retains successful status"
               0 status))

(multiple-value-bind (text status)
    (cclsh:capture
      (check-run-program "/bin/sh" "-c" "exit 37"))
  (check-equal "nonzero nested run produces no capture" "" text)
  (check-equal "nested run status becomes builtin stage status" 37 status))

(multiple-value-bind (text status)
    (cclsh:capture (check-run-order))
  (check-equal "nested run preserves Lisp and child output order"
               "before|child|after|"
               text)
  (check-equal "ordered nested run retains successful status" 0 status))


;;;; -- Implicit directory commands --

(let* ((root
         (format nil "/tmp/cclsh-check-implicit-cd-~d/"
                 (ccl:external-call "getpid" :int)))
       (child         (concatenate 'string root "implicit-child/"))
       (spaced        (concatenate 'string root "implicit-space dir/"))
       (old-directory (ccl:current-directory))
       (old-defaults  *default-pathname-defaults*)
       (old-pwd       (cclsh:getenv "PWD"))
       (old-oldpwd    (cclsh:getenv "OLDPWD"))
       (old-home      (cclsh:getenv "HOME"))
       (variable      "CCLSH_CHECK_IMPLICIT_DIR")
       (old-variable  (cclsh:getenv variable)))
  (flet ((current-directory-name ()
           (cclsh::directory-namestring-clean (ccl:current-directory)))
         (restore-environment (name value)
           (if value
               (cclsh:setenv name value)
               (cclsh::unsetenv name))))
    (unwind-protect
        (let ((*package* (find-package '#:cclsh-user))
              (*standard-output* (make-broadcast-stream))
              (*error-output* (make-broadcast-stream)))
          (ensure-directories-exist
           (concatenate 'string child ".keep"))
          (ensure-directories-exist
           (concatenate 'string spaced ".keep"))
          (ensure-directories-exist
           (concatenate 'string root "rehash/.keep"))
          (ensure-directories-exist
           (concatenate 'string root "true/.keep"))
          (check-write-utf8-file
           (concatenate 'string root "implicit-plain") "plain")
          (cclsh:cd root)

          (cclsh:cd child)
          (check-equal ".. changes to the parent directory"
                       0
                       (cclsh::dispatch-line ".."))
          (check-equal ".. changes the process directory"
                       (string-right-trim "/" root)
                       (current-directory-name))
          (check-equal "implicit cd updates PWD"
                       (string-right-trim "/" root)
                       (cclsh:getenv "PWD"))
          (check-equal "implicit cd updates OLDPWD"
                       (string-right-trim "/" child)
                       (cclsh:getenv "OLDPWD"))

          (check-equal "./ directory changes implicitly"
                       0
                       (cclsh::dispatch-line "./implicit-child"))
          (check-equal "./ directory becomes current"
                       (string-right-trim "/" child)
                       (current-directory-name))
          (cclsh:cd root)
          (check-equal "trailing slash directory changes implicitly"
                       0
                       (cclsh::dispatch-line "implicit-child/"))
          (check-equal "trailing slash directory becomes current"
                       (string-right-trim "/" child)
                       (current-directory-name))
          (cclsh:cd root)
          (check-equal "absolute directory changes implicitly"
                       0
                       (cclsh::dispatch-line
                        (string-right-trim "/" child)))
          (check-equal "absolute directory becomes current"
                       (string-right-trim "/" child)
                       (current-directory-name))
          (cclsh:cd root)
          (cclsh:setenv "HOME" (string-right-trim "/" root))
          (check-equal "tilde directory changes implicitly"
                       0
                       (cclsh::dispatch-line "~/implicit-child"))
          (check-equal "tilde directory becomes current"
                       (string-right-trim "/" child)
                       (current-directory-name))
          (cclsh:cd root)
          (cclsh:setenv variable (string-right-trim "/" child))
          (check-equal "variable directory changes implicitly"
                       0
                       (cclsh::dispatch-line
                        "$CCLSH_CHECK_IMPLICIT_DIR"))
          (check-equal "variable directory becomes current"
                       (string-right-trim "/" child)
                       (current-directory-name))
          (cclsh:cd root)
          (check-equal "singleton directory glob changes implicitly"
                       0
                       (cclsh::dispatch-line "./implicit-chil?"))
          (check-equal "singleton glob directory becomes current"
                       (string-right-trim "/" child)
                       (current-directory-name))
          (cclsh:cd root)
          (check-equal "quoted directory changes implicitly"
                       0
                       (cclsh::dispatch-line "'implicit-space dir/'"))
          (check-equal "quoted directory becomes current"
                       (string-right-trim "/" spaced)
                       (current-directory-name))

          (cclsh:cd root)
          (check-equal "bare directory remains a command lookup"
                       127
                       (cclsh::dispatch-line "implicit-child"))
          (check-equal "bare directory does not change directory"
                       (string-right-trim "/" root)
                       (current-directory-name))
          (check-equal "implicit directory rejects arguments"
                       127
                       (cclsh::dispatch-line "implicit-child/ extra"))
          (check-equal "directory arguments do not change directory"
                       (string-right-trim "/" root)
                       (current-directory-name))
          (check-equal "implicit directory rejects backgrounding"
                       1
                       (cclsh::dispatch-line "implicit-child/ &"))
          (check-equal "background rejection does not change directory"
                       (string-right-trim "/" root)
                       (current-directory-name))
          (check-equal "builtin wins over a same-named directory"
                       0
                       (cclsh::dispatch-line "rehash"))
          (check-equal "builtin precedence keeps the directory"
                       (string-right-trim "/" root)
                       (current-directory-name))
          (check-equal "external wins over a same-named directory"
                       0
                       (cclsh::dispatch-line "true"))
          (check-equal "external precedence keeps the directory"
                       (string-right-trim "/" root)
                       (current-directory-name))

          (check-equal "implicit directory highlights like cd"
                       ':cyan
                       (cclsh::highlight-command-name
                        "implicit-child/" t))
          (check-equal "bare directory remains unknown highlighting"
                       ':red
                       (cclsh::highlight-command-name
                        "implicit-child" t))
          (check-equal "directory with arguments remains unknown"
                       ':red
                       (cclsh::highlight-command-name
                        "implicit-child/" nil))
          (check-equal "multi-match directory glob remains unknown"
                       ':red
                       (cclsh::highlight-command-name "./implicit-*" t))
          (multiple-value-bind (start candidates displays)
              (cclsh::complete-line "implicit-" 9)
            (declare (ignore displays))
            (check-equal "command-position directory completion start"
                         0 start)
            (check-equal "command-position completion includes directory"
                         t
                         (not (null
                               (member "implicit-child/" candidates
                                       :test #'string=))))
            (check-equal "command-position completion excludes plain file"
                         nil
                         (member "implicit-plain" candidates
                                 :test #'string=))))
      (ignore-errors
        (setf (ccl:current-directory) old-directory)
        (setf *default-pathname-defaults* old-defaults))
      (restore-environment "PWD" old-pwd)
      (restore-environment "OLDPWD" old-oldpwd)
      (restore-environment "HOME" old-home)
      (restore-environment variable old-variable)
      (ignore-errors
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist ':ignore)))))


;;;; -- Directory-change hooks --

(let* ((root
         (format nil "/tmp/cclsh-check-directory-hooks-~d/"
                 (ccl:external-call "getpid" :int)))
       (child         (concatenate 'string root "child/"))
       (nested        (concatenate 'string root "nested/"))
       (missing       (concatenate 'string root "missing/"))
       (old-directory (ccl:current-directory))
       (old-defaults  *default-pathname-defaults*)
       (old-pwd       (cclsh:getenv "PWD"))
       (old-oldpwd    (cclsh:getenv "OLDPWD"))
       (events        nil))
  (flet ((restore-environment (name value)
           (if value
               (cclsh:setenv name value)
               (cclsh::unsetenv name))))
    (unwind-protect
        (let ((cclsh:*directory-change-hooks* nil)
              (*error-output* (make-broadcast-stream)))
          (ensure-directories-exist
           (concatenate 'string child ".keep"))
          (ensure-directories-exist
           (concatenate 'string nested ".keep"))
          (cclsh:cd root)
          (labels ((record-state (old new)
                     (push (list ':first old new
                                 (cclsh:getenv "OLDPWD")
                                 (cclsh:getenv "PWD")
                                 (cclsh::directory-namestring-clean
                                  *default-pathname-defaults*))
                           events))

                   (fail-after-recording (old new)
                     (declare (ignore old new))
                     (push (list ':failing) events)
                     (error "hook failure"))

                   (record-later (old new)
                     (declare (ignore old new))
                     (push (list ':later) events)))
            (cclsh:directory-change-hook-add #'record-state)
            (cclsh:directory-change-hook-add #'fail-after-recording)
            (cclsh:directory-change-hook-add #'record-later)
            (cclsh:directory-change-hook-add #'record-state)
            (check-equal "directory hook registration is idempotent"
                         3
                         (length cclsh:*directory-change-hooks*))
            (check-equal "successful cd survives a failing hook"
                         0
                         (cclsh:cd child))
            (let ((ordered (nreverse events)))
              (check-equal "directory hooks retain registration order"
                           '(:first :failing :later)
                           (mapcar #'first ordered))
              (check-equal "directory hook sees committed state"
                           (list ':first
                                 (string-right-trim "/" root)
                                 (string-right-trim "/" child)
                                 (string-right-trim "/" root)
                                 (string-right-trim "/" child)
                                 (string-right-trim "/" child))
                           (first ordered)))
            (setf events nil)
            (check-equal "same-directory cd succeeds"
                         0
                         (cclsh:cd child))
            (check-equal "same-directory cd does not run hooks"
                         nil events)
            (check-equal "failed cd reports failure"
                         1
                         (cclsh:cd missing))
            (check-equal "failed cd does not run hooks"
                         nil events)
            (cclsh:directory-change-hook-remove #'record-later)
            (check-equal "directory hook removal takes effect"
                         2
                         (length cclsh:*directory-change-hooks*)))
          (let ((nested-status nil)
                (later-pwd nil)
                (cclsh:*directory-change-hooks* nil))
            (cclsh:cd root)
            (labels ((change-again (old new)
                       (declare (ignore old new))
                       (setf nested-status (cclsh:cd nested)))

                     (record-final-pwd (old new)
                       (declare (ignore old new))
                       (setf later-pwd (cclsh:getenv "PWD"))))
              (cclsh:directory-change-hook-add #'change-again)
              (cclsh:directory-change-hook-add #'record-final-pwd)
              (check-equal "outer cd survives a reentrant hook"
                           0
                           (cclsh:cd child))
              (check-equal "directory hooks reject a reentrant cd"
                           1 nested-status)
              (check-equal "reentrant cd leaves committed state coherent"
                           (string-right-trim "/" child)
                           later-pwd))))
      (ignore-errors
        (setf (ccl:current-directory) old-directory))
      (setf *default-pathname-defaults* old-defaults)
      (restore-environment "PWD" old-pwd)
      (restore-environment "OLDPWD" old-oldpwd)
      (ignore-errors
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist ':ignore)))))


;;;; -- History --

(let* ((multiline (format nil "echo one~%echo two"))
       (history   (check-history "git status" "echo longer" "echo")))
  (check-equal "newest longer prefix"
               "echo newest"
               (cclsh::history-suggestion
                "ec" (check-history "echo older" "echo newest")))
  (check-equal "skip equal newest entry"
               "echo longer"
               (cclsh::history-suggestion "echo" history))
  (check-equal "empty input has no suggestion"
               nil
               (cclsh::history-suggestion "" history))
  (check-equal "history matching is case-sensitive"
               nil
               (cclsh::history-suggestion "E" history))
  (check-equal "multiline suggestion remains exact"
               multiline
               (cclsh::history-suggestion
                "echo one" (check-history multiline)))
  (check-equal "history search matches a middle substring"
               t
               (cclsh::history-search-match-p
                "status" "git status --short"))
  (check-equal "lowercase history search ignores case"
               t
               (cclsh::history-search-match-p "git" "GIT LOG"))
  (check-equal "uppercase history search is case-sensitive"
               nil
               (cclsh::history-search-match-p "Git" "git log"))
  (check-equal "uppercase history search accepts exact case"
               t
               (cclsh::history-search-match-p "Git" "run Git log"))
  (check-equal "history search rejects missing substrings"
               nil
               (cclsh::history-search-match-p "branch" "git status"))
  (check-equal "printed history string round-trips"
               multiline
               (read-from-string
                (with-output-to-string (stream)
                  (prin1 multiline stream)))))

(let* ((root (format nil "/tmp/cclsh-check-žluť-你好-~d/"
                     (ccl:external-call "getpid" :int)))
       (old-xdg (cclsh:getenv "XDG_CONFIG_HOME"))
       (text "Příliš žluťoučký kůň 🐈")
       (startup-symbol
         (intern "*CHECK-UTF8-STARTUP*" (find-package '#:cclsh-user))))
  (unwind-protect
      (let ((cclsh::*history*
              (make-array 0 :adjustable t :fill-pointer t))
            (ccl:*default-file-character-encoding* ':iso-8859-1))
        (cclsh:setenv "XDG_CONFIG_HOME" root)
        (let ((old-umask
                (ccl:external-call "umask"
                                   :unsigned-int 0
                                   :unsigned-int)))
          (unwind-protect
              (cclsh::history-append text)
            (ccl:external-call "umask"
                               :unsigned-int old-umask
                               :unsigned-int)))
        (check-equal "history configuration is private"
                     "700"
                     (check-path-mode (cclsh::config-directory)))
        (check-equal "history file is private"
                     "600"
                     (check-path-mode (cclsh::history-file)))
        (setf (fill-pointer cclsh::*history*) 0)
        (cclsh::history-load)
        (check-equal "UTF-8 history ignores the ambient file encoding"
                     text
                     (and (= (fill-pointer cclsh::*history*) 1)
                          (aref cclsh::*history* 0)))
         (let ((cclsh::*history-limit* 2))
           (check-write-utf8-file
            (cclsh::history-file)
            (with-output-to-string (stream)
              (dolist (entry '("old" "middle" "new"))
                (prin1 entry stream)
                (terpri stream))))
           (cclsh::history-load)
           (check-equal "history load retains only the newest bounded entries"
                        '("middle" "new")
                        (coerce cclsh::*history* 'list)))
        (makunbound startup-symbol)
        (check-write-utf8-file
         (cclsh::startup-file)
         (format nil
                 "(setf (symbol-value (find-symbol ~s ~s)) ~s)~%"
                 (symbol-name startup-symbol) "CCLSH-USER" text))
        (cclsh::startup-load)
        (check-equal "UTF-8 startup ignores the ambient file encoding"
                     text
                     (and (boundp startup-symbol)
                          (symbol-value startup-symbol))))
    (when (boundp startup-symbol)
      (makunbound startup-symbol))
    (if old-xdg
        (cclsh:setenv "XDG_CONFIG_HOME" old-xdg)
        (cclsh::unsetenv "XDG_CONFIG_HOME"))
    (ignore-errors
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore))))


;;;; -- Current package environment --

(let* ((variable     "CCLSH_PACKAGE")
       (package-name  (format nil "CCLSH-CHECK-猫-PACKAGE-~d"
                              (ccl:external-call "getpid" :int)))
       (package       (make-package package-name :use nil))
       (old-variable  (cclsh:getenv variable)))
  (unwind-protect
      (let ((*package* package))
        (cclsh:setenv variable "STALE")
        (check-equal "package sync returns the canonical name"
                     package-name
                     (cclsh::environment-package-sync))
        (check-equal "package sync updates CCLSH_PACKAGE"
                     package-name
                     (cclsh:getenv variable))

        (cclsh:setenv variable "STALE")
        (check-equal "environment snapshot refreshes CCLSH_PACKAGE"
                     t
                     (not
                      (null
                       (find (format nil "~a=~a" variable package-name)
                             (cclsh:environment-variables)
                             :test #'string=))))

        (cclsh:setenv variable "STALE")
        (multiple-value-bind (output status)
            (cclsh::pipeline-capture
             (list (list "/usr/bin/printenv" variable)))
          (check-equal "external child receives current Lisp package"
                       0 status)
          (check-equal "external child package value is exact"
                       package-name output)))
    (delete-package package)
    (if old-variable
        (cclsh:setenv variable old-variable)
        (cclsh::unsetenv variable))))


;;;; -- UTF-8 environment --

(let* ((name  "CCLSH_CHECK_ŽLUŤ")
       (value "Příliš žluťoučký kůň 🐈")
       (old   (cclsh:getenv name)))
  (unwind-protect
      (progn
        (cclsh:setenv name value)
        (check-equal "UTF-8 getenv round-trip"
                     value
                     (cclsh:getenv name))
        (check-equal "UTF-8 environ snapshot"
                     t
                     (not (null
                           (find (format nil "~a=~a" name value)
                                 (cclsh:environment-variables)
                                 :test #'string=))))
        (cclsh::unsetenv name)
        (check-equal "UTF-8 unsetenv"
                     nil
                     (cclsh:getenv name)))
    (if old
        (cclsh:setenv name old)
        (cclsh::unsetenv name))))

(let* ((old-locale (check-set-locale nil))
       (czech-locale (or (check-set-locale "cs_CZ.UTF-8")
                         (check-set-locale "cs_CZ.utf8")))
       (expected "Adresář nebo soubor neexistuje"))
  (unwind-protect
      (when czech-locale
        (check-equal "UTF-8 libc error text"
                     expected
                     (cclsh::process--error-string 2))
        (check-equal "UTF-8 environment error report"
                     t
                     (not (null
                           (search expected
                                   (princ-to-string
                                    (make-condition
                                     'cclsh::environment-error
                                     :operation "set"
                                     :name "BROKEN"
                                     :code 2))))))
        (check-equal "UTF-8 terminal error report"
                     t
                     (not (null
                           (search expected
                                   (princ-to-string
                                    (make-condition
                                     'cclsh::terminal-control-error
                                     :operation "foreground"
                                     :process-group 1
                                     :code 2)))))))
    (when old-locale
      (check-set-locale old-locale))))


;;;; -- Lisp-dispatched shell status --

(let ((cclsh:*last-status* 23)
      (cclsh::*jobs-exit-warned* t)
      (*standard-output* (make-string-output-stream))
      (*error-output* (make-string-output-stream)))
  (check-equal "comment preserves the previous status"
               23
               (cclsh::dispatch-line "  ; not a command"))
  (check-equal "comment leaves the previous status recorded"
               23
               cclsh:*last-status*)
  (check-equal "comment leaves stopped-job exit confirmation armed"
               t
               cclsh::*jobs-exit-warned*)
  (check-equal "comment writes no standard output"
               ""
               (get-output-stream-string *standard-output*))
  (check-equal "comment writes no error output"
               ""
               (get-output-stream-string *error-output*)))

(let ((cclsh:*last-status* 23)
      (*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (check-equal "ordinary successful Lisp returns zero"
               0
               (cclsh::dispatch-line "(+ 20 22)"))
  (check-equal "ordinary successful Lisp records zero"
               0
               cclsh:*last-status*))

(let ((cclsh:*last-status* 23)
      (*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (check-equal "same shell status survives Lisp dispatch"
               23
               (cclsh::dispatch-line
                "(cclsh::command-status-record 23)"))
  (check-equal "same shell status remains recorded"
               23
               cclsh:*last-status*))

(let ((cclsh:*last-status* 0)
      (*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (check-equal "shell status survives later Lisp forms"
               17
               (cclsh::dispatch-line
                "(cclsh::command-status-record 17) (+ 1 1)"))
  (check-equal "later Lisp forms retain recorded shell status"
               17
               cclsh:*last-status*))

(let ((cclsh:*last-status* 0)
      (*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (check-equal "Lisp error overrides an earlier shell status"
               1
               (cclsh::dispatch-line
                "(progn (cclsh::command-status-record 17) (error \"no\"))"))
  (check-equal "Lisp error records failure after shell status"
               1
               cclsh:*last-status*))


;;;; -- Job aggregation --

(let ((process (cclsh::process--make 1)))
  (check-equal "new process starts at transition generation zero"
               '(:running nil 0)
               (multiple-value-list (cclsh::shell-process-status process)))
  (cclsh::process--publish-state
   process ':stopped cclsh::+process-sigtstp+)
  (check-equal "process stop advances its transition generation"
               (list ':stopped cclsh::+process-sigtstp+ 1)
               (multiple-value-list (cclsh::shell-process-status process)))
  (cclsh::process--publish-state process ':running nil)
  (check-equal "process continuation advances its transition generation"
               '(:running nil 2)
               (multiple-value-list (cclsh::shell-process-status process))))

(check-equal "stopped external group wins over a live Lisp task"
             ':stopped
             (cclsh::job--aggregate-state '(:done :stopped) nil))
(check-equal "finished externals wait for a live Lisp task"
             ':running
             (cclsh::job--aggregate-state '(:done :done) nil))
(check-equal "job finishes after external and Lisp tasks"
             ':done
             (cclsh::job--aggregate-state '(:done :done) t))

(let ((done nil)
      (job  (cclsh::job-make)))
  (cclsh::job-add-auxiliary job (lambda () done))
  (check-equal "live auxiliary keeps an empty job running"
               ':running
               (cclsh::job-refresh job))
  (setf done t)
  (check-equal "completed auxiliary lets an empty job finish"
               ':done
               (cclsh::job-refresh job)))

(let ((job
        (cclsh::job-make
         :result-provider (lambda () (values ':signaled 2)))))
  (setf (cclsh::job-status job) ':done)
  (check-equal "logical final stage supplies resumed job status"
               130
               (cclsh::job-exit-status job))
  (check-equal "logical final stage supplies job display status"
               "Interrupt"
               (cclsh::job--status-text job)))

(let* ((callers      20)
       (job          (cclsh::job-make :command "concurrent startup"))
       (event        (cclsh::job-event job))
       (process      nil)
       (start-gate   (ccl:make-semaphore))
       (all-returned (ccl:make-semaphore))
       (result-lock  (ccl:make-lock "monitor startup results"))
       (returned     0)
       (failures     nil)
       (threads      nil))
  (unwind-protect
      (progn
        ;; Keep the child alive so child events cannot accidentally wake
        ;; startup callers stranded on the job's transition semaphore.
        (setf process
              (cclsh::shell-process-spawn
               "/usr/bin/sleep" (list "30")
               :process-group 0 :event event))
        (setf (cclsh::job-processes job) (list process)
              (cclsh::job-process-group job)
              (cclsh::shell-process-pid process))
        (loop repeat callers
              do (push
                  (ccl:process-run-function
                   "concurrent monitor starter"
                   (lambda ()
                     (ccl:wait-on-semaphore start-gate)
                     (let ((failure nil))
                       (handler-case
                           (cclsh::job-start-monitors job)
                         (error (condition)
                           (setf failure condition)))
                       (ccl:with-lock-grabbed (result-lock)
                         (when failure
                           (push (princ-to-string failure) failures))
                         (incf returned)
                         (when (= returned callers)
                           (ccl:signal-semaphore all-returned))))))
                  threads))
        (loop repeat callers
              do (ccl:signal-semaphore start-gate))
        (ccl:timed-wait-on-semaphore all-returned 3)
        (check-equal "concurrent monitor startup returns every caller"
                     callers
                     (ccl:with-lock-grabbed (result-lock) returned))
        (check-equal "concurrent monitor startup has no failures"
                     nil
                     (ccl:with-lock-grabbed (result-lock)
                       (copy-list failures))))
    (dolist (thread threads)
      (when (ccl::process-active-p thread)
        (ignore-errors (ccl:process-kill thread)))
      (ignore-errors (ccl:join-process thread)))
    (when process
      (ignore-errors (cclsh::job--kill-reap job)))))

(let* ((started (ccl:make-semaphore))
       (blocker (ccl:make-semaphore))
       (job     (cclsh::job-make :command "anchor signal"))
       (group   (cclsh::make-pipeline-task-group :job job))
       (anchor  (cclsh::process--make 1))
       (task
         (cclsh::make-pipeline-task
          :name "anchor-controlled task"
          :group group
          :function (lambda ()
                      (ccl:signal-semaphore started)
                      (ccl:wait-on-semaphore blocker)
                      (values 0 nil))))
       (thread nil))
  (setf (cclsh::pipeline-task-group-tasks group) (list task)
        (cclsh::pipeline-task-group-anchor-process group) anchor)
  (unwind-protect
      (progn
        (setf thread
              (ccl:process-run-function
               "anchor-controlled task"
               #'cclsh::pipeline--run-task task)
              (cclsh::pipeline-task-thread task) thread)
        (check-equal "anchor-controlled task starts"
                     t
                     (not (null
                           (ccl:timed-wait-on-semaphore started 2))))
        (ccl:with-lock-grabbed ((cclsh::shell-process-lock anchor))
          (setf (cclsh::shell-process-state anchor) ':signaled
                (cclsh::shell-process-code anchor)
                cclsh::+process-sigpipe+))
        (cclsh::pipeline--tasks-lifecycle-done-p group)
        (check-equal "anchor SIGPIPE does not cancel Lisp tasks"
                     nil
                     (cclsh::pipeline-task-group-aborted group))
        (ccl:with-lock-grabbed ((cclsh::shell-process-lock anchor))
          (setf (cclsh::shell-process-code anchor) 15))
        (cclsh::pipeline--tasks-lifecycle-done-p group)
        (ccl:join-process thread)
        (setf thread nil)
        (check-equal "fatal anchor signal completes Lisp task"
                     t
                     (cclsh::pipeline-task-done task))
        (check-equal "fatal anchor signal supplies Lisp task state"
                     ':signaled
                     (cclsh::pipeline-task-state task))
        (check-equal "fatal anchor signal supplies Lisp task code"
                     15
                     (cclsh::pipeline-task-code task)))
    (when (and thread (ccl::process-active-p thread))
      (ignore-errors
        (cclsh::pipeline--abort-tasks group :signal 9))
      (ignore-errors (ccl:join-process thread)))))


;;;; -- Declarative argument completion --

(defvar *check-completion-context* nil
  "Last context received by the declarative completion test callback.")

(defvar *check-completion-side-effect* nil
  "Sentinel proving completion does not evaluate Lisp substitutions.")

(defun check-completion-dynamic-choices (argument context)
  "Return context-sensitive choices for completion regression checks."
  (declare (ignore argument context))
  '("east" "west"))

(defun check-completion-custom-provider (argument context)
  "Record CONTEXT and return candidates with parallel descriptions."
  (declare (ignore argument))
  (setf *check-completion-context* context)
  (values '("custom alpha" "custom-beta")
          '("First custom choice" "Second custom choice")))

(defun check-completion-failing-provider (argument context)
  "Signal an injected provider failure that interactive completion must contain."
  (declare (ignore argument context))
  (error "injected completion provider failure"))

(defun check-completion-rich-provider (argument context)
  "Return one rich candidate through the public semantic record protocol."
  (declare (ignore argument context))
  (list (cclsh:make-completion-candidate
         :insertion "rich\\ value"
         :display "rich value"
         :description "A rich provider value."
         :kind ':demonstration)))

(defun check-completion-negative-provider (argument context)
  "Return negative integers to distinguish them from option prefixes."
  (declare (ignore argument context))
  '("-10" "-20"))

(cclsh:defcommand cclsh-user::check-completion-declarative
    (mode &key output directory verbose quiet dynamic custom failing
               rich package-name program environment job count label)
  "Expose every declarative completion kind to the regression suite."
  (:arguments
   (mode         :choices ("fast" "safe" "two words" :turbo)
                 :help "Execution mode.")
   (output       :type :pathname :short #\o
                 :help "Output path.")
   (directory    :type :directory
                 :help "Working directory.")
   (verbose      :type :boolean :short #\v
                 :help "Print detailed progress.")
   (quiet        :type :boolean :short #\q
                 :help "Suppress ordinary output.")
   (dynamic      :choices #'check-completion-dynamic-choices
                 :short #\d :help "A dynamic direction.")
   (custom       :completion #'check-completion-custom-provider
                 :short #\c :help "A provider-defined value.")
   (failing      :completion #'check-completion-failing-provider
                 :help "A deliberately failing provider.")
   (rich         :completion #'check-completion-rich-provider
                 :help "A rich provider-defined value.")
   (package-name :type :package :help "A loaded Lisp package.")
   (program      :type :command :help "A shell command.")
   (environment  :type :environment-variable
                 :help "An environment-variable name.")
   (job          :type :job :help "A shell job selector.")
   (count        :type :integer :help "An integer without candidates.")
   (label        :type :string :help "A string without candidates."))
  (declare (ignore mode output directory verbose quiet dynamic custom failing
                   rich package-name program environment job count label))
  0)

(cclsh:defcommand cclsh-user::check-completion-repeat (&rest modes)
  "Expose a repeating positional choice to completion."
  (:arguments
   (modes :choices ("alpha" "beta") :help "One or more modes."))
  (declare (ignore modes))
  0)

(cclsh:defcommand cclsh-user::check-completion-negative (number)
  "Expose a numeric positional with a custom semantic provider."
  (:arguments
   (number :type :integer :completion #'check-completion-negative-provider
           :help "A negative integer."))
  (declare (ignore number))
  0)

(cclsh:defcommand cclsh-user::check-completion-legacy (&rest arguments)
  "Retain generic file completion without an :ARGUMENTS declaration."
  (declare (ignore arguments))
  0)

(defun check-completion (line &optional (cursor (length line)))
  "Return COMPLETE-LINE's values for LINE in the interactive user package."
  (let ((*package* (find-package '#:cclsh-user)))
    (multiple-value-list (cclsh::complete-line line cursor))))

(defun check-completion-candidate-p (candidate result)
  "True when CANDIDATE appears in completion RESULT."
  (not (null (member candidate (second result) :test #'string=))))

(let* ((command "check-completion-declarative")
       (choice-line (format nil "~a f" command))
       (choice-result (check-completion choice-line)))
  (check-equal "positional choices use the active argument"
               t
               (check-completion-candidate-p "fast" choice-result))
  (let* ((spaced-line (format nil "~a t" command))
         (spaced-result (check-completion spaced-line)))
    (check-equal "choice completion escapes shell separators"
                 t
                 (check-completion-candidate-p "two\\ words" spaced-result))
    (check-equal "escaped choice completion preserves its argument"
                 '("two words")
                 (cclsh::command-line-words "two\\ words")))
  (let ((symbol-result
          (check-completion (format nil "~a :" command))))
    (check-equal "symbol choices retain their printed argument syntax"
                 t
                 (check-completion-candidate-p ":TURBO" symbol-result)))
  (let* ((quoted-line (format nil "~a ~cf" command #\"))
         (quoted-result (check-completion quoted-line)))
    (check-equal "open quoted argument completes from the whole safe group"
                 (1+ (length command))
                 (first quoted-result))
    (check-equal "open quoted choice includes its semantic candidate"
                 t
                 (check-completion-candidate-p "fast" quoted-result)))
  (let* ((earlier-line (format nil "~a f --verbose" command))
         (cursor (+ (length command) 2))
         (earlier-result (check-completion earlier-line cursor)))
    (check-equal "completion parses only words preceding an earlier cursor"
                 t
                 (check-completion-candidate-p "fast" earlier-result)))
  (let* ((inside-line (format nil "~a fast" command))
         (cursor (1- (length inside-line)))
         (inside-result (check-completion inside-line cursor)))
    (check-equal "completion inside an argument does not retain a stale suffix"
                 nil
                 (second inside-result)))
  (let ((interspersed-result
          (check-completion (format nil "~a --verbose f" command))))
    (check-equal "interspersed options retain the next positional argument"
                 t
                 (check-completion-candidate-p
                  "fast" interspersed-result)))
  (let ((terminated-choice-result
          (check-completion (format nil "~a -- f" command))))
    (check-equal "the option terminator retains positional completion"
                 t
                 (check-completion-candidate-p
                  "fast" terminated-choice-result))))

(let* ((command "check-completion-declarative")
       (option-result (check-completion (format nil "~a --v" command)))
       (option-index (position "--verbose" (second option-result)
                               :test #'string=)))
  (check-equal "long option completion uses declarative names"
               t
               (not (null option-index)))
  (check-equal "option completion displays declarative help"
               t
               (and option-index
                    (not (null
                          (search "Print detailed progress."
                                  (nth option-index (third option-result)))))))
  (let ((short-result (check-completion (format nil "~a -v" command))))
    (check-equal "short option completion uses declarative names"
                 t
                 (check-completion-candidate-p "-v" short-result)))
  (let ((help-result (check-completion (format nil "~a --h" command))))
    (check-equal "generated help participates in option completion"
                 t
                 (check-completion-candidate-p "--help" help-result))
    (check-equal "generated help completion has a description"
                 t
                 (not (null
                       (search "Show command help."
                               (first (third help-result)))))))
  (let ((used-result
          (check-completion (format nil "~a --verbose --v" command))))
    (check-equal "non-repeating used options are not offered again"
                 nil
                 (member "--verbose" (second used-result) :test #'string=)))
  (let ((terminated-result
          (check-completion (format nil "~a -- --v" command))))
    (check-equal "option terminator prevents later option completion"
                 nil
                 (member "--verbose" (second terminated-result)
                         :test #'string=)))
  (let ((value-result
          (check-completion (format nil "~a fast --output -- -" command))))
    (check-equal "double dash consumed as a value leaves options enabled"
                 t
                 (check-completion-candidate-p "--verbose" value-result)))
  (let ((help-value-result
          (check-completion
           (format nil "~a fast --output --help --v" command))))
    (check-equal "--help consumed as an option value does not suppress completion"
                 t
                 (check-completion-candidate-p
                  "--verbose" help-value-result))))

(let* ((command "check-completion-declarative")
       (dynamic-line (format nil "~a fast --dynamic e" command))
       (dynamic-result (check-completion dynamic-line)))
  (check-equal "dynamic choice functions supply option values"
               t
               (check-completion-candidate-p "east" dynamic-result))
  (setf *check-completion-context* nil)
  (let* ((custom-line
           (format nil "~a fast --verbose --custom custom" command))
         (custom-result (check-completion custom-line))
         (context *check-completion-context*)
         (custom-index (position "custom\\ alpha" (second custom-result)
                                 :test #'string=)))
    (check-equal "custom providers return safely escaped candidates"
                 t
                 (not (null custom-index)))
    (check-equal "custom provider descriptions reach the selector display"
                 t
                 (and custom-index
                      (not (null
                            (search "First custom choice"
                                    (nth custom-index
                                         (third custom-result)))))))
    (check-equal "custom provider receives the active prefix"
                 "custom"
                 (cclsh:command-completion-context-prefix context))
    (check-equal "custom provider receives preceding command words"
                 '("fast" "--verbose" "--custom")
                 (cclsh:command-completion-context-words context))
    (check-equal "custom provider receives the positional index"
                 1
                 (cclsh:command-completion-context-positional-index context))
    (check-equal "pending option is the provider's active argument"
                 'custom
                 (cclsh:command-argument-name
                  (cclsh:command-completion-context-argument context)))
    (check-equal "custom provider receives completed used options"
                 '(verbose)
                 (mapcar #'cclsh:command-argument-name
                         (cclsh:command-completion-context-used-options
                          context))))
  (let ((failing-result
          (check-completion (format nil "~a fast --failing x" command))))
    (check-equal "provider failures degrade to no candidates"
                 nil
                 (second failing-result)))
  (setf *check-completion-context* nil)
  (let* ((attached-result
           (check-completion (format nil "~a fast -qvccustom" command)))
         (attached-context *check-completion-context*))
    (check-equal "an attached custom value keeps its semantic candidates"
                 t
                 (check-completion-candidate-p
                  "custom\\ alpha" attached-result))
    (check-equal "grouped flags before an attached value enter provider context"
                 '(quiet verbose)
                 (mapcar #'cclsh:command-argument-name
                         (cclsh:command-completion-context-used-options
                          attached-context))))
  (let* ((rich-result
           (check-completion (format nil "~a fast --rich rich" command)))
         (rich-index
           (position "rich\\ value" (second rich-result) :test #'string=)))
    (check-equal "rich provider records preserve insertion text"
                 t
                 (not (null rich-index)))
    (check-equal "rich provider records preserve descriptions"
                 t
                 (and rich-index
                      (not (null
                            (search "A rich provider value."
                                    (nth rich-index
                                         (third rich-result)))))))))

(let ((repeat-result
        (check-completion "check-completion-repeat alpha b")))
  (check-equal "repeating positional arguments remain active"
               t
               (check-completion-candidate-p "beta" repeat-result)))

(let ((minus-result
        (check-completion "check-completion-negative -"))
      (negative-result
        (check-completion "check-completion-negative -1")))
  (check-equal "a lone dash follows execution's positional treatment"
               t
               (check-completion-candidate-p "-10" minus-result))
  (check-equal "negative numeric prefixes do not become short options"
               t
               (check-completion-candidate-p "-10" negative-result)))

(let ((package-result
        (check-completion
         "check-completion-declarative fast --package-name cclsh-"))
      (command-result
        (check-completion
         "check-completion-declarative fast --program reh"))
      (integer-result
        (check-completion
         "check-completion-declarative fast --count 1"))
      (string-result
        (check-completion
         "check-completion-declarative fast --label x")))
  (check-equal "package arguments complete loaded packages"
               t
               (check-completion-candidate-p "cclsh-user" package-result))
  (check-equal "command arguments complete shell commands"
               t
               (check-completion-candidate-p "rehash" command-result))
  (check-equal "semantic candidate displays include argument help"
               t
               (not (null
                     (find-if (lambda (display)
                                (search "A shell command." display))
                              (third command-result)))))
  (check-equal "integer arguments intentionally have no candidates"
               nil
               (second integer-result))
  (check-equal "string arguments intentionally have no candidates"
               nil
               (second string-result)))

(let* ((name "CCLSH_COMPLETION_AUDIT_NAME")
       (old-value (cclsh:getenv name)))
  (unwind-protect
      (progn
        (cclsh:setenv name "not displayed")
        (let ((result
                (check-completion
                 "check-completion-declarative fast --environment CCLSH_COMPLETION_A")))
          (check-equal "environment arguments complete variable names"
                       t
                       (check-completion-candidate-p name result))
          (check-equal "environment completion does not expose values"
                       nil
                       (find-if (lambda (display)
                                  (search "not displayed" display))
                                (third result)))))
    (if old-value
        (cclsh:setenv name old-value)
        (cclsh::unsetenv name))))

(let* ((current  (cclsh::job-make :command "sleep current"))
       (previous (cclsh::job-make :command "deploy worker beta"))
       (other    (cclsh::job-make :command "sleep other")))
  (setf (cclsh::job-id current) 7
        (cclsh::job-touched current) 30
        (cclsh::job-id previous) 3
        (cclsh::job-touched previous) 20
        (cclsh::job-id other) 11
        (cclsh::job-touched other) 10)
  (let ((cclsh::*jobs* (list current previous other)))
    (check-equal "disown accepts a numeric job id"
                 0
                 (cclsh:disown 11))
    (check-equal "numeric disown removes only its job"
                 (list current previous)
                 cclsh::*jobs*)
    (check-equal "disown accepts a command substring"
                 0
                 (cclsh:disown "worker beta"))
    (check-equal "substring disown removes its job"
                 (list current)
                 cclsh::*jobs*)
    (check-equal "disown without an argument removes the current job"
                 0
                 (cclsh:disown))
    (check-equal "disown empties the job table"
                 nil
                 cclsh::*jobs*)
    (let ((*error-output* (make-string-output-stream)))
      (check-equal "disown rejects an unknown job"
                   1
                   (cclsh:disown "missing")))))

(let* ((current (cclsh::job-make :command "sleep current"))
       (previous (cclsh::job-make :command "sleep previous")))
  (setf (cclsh::job-id current) 7
        (cclsh::job-status current) ':running
        (cclsh::job-touched current) 20
        (cclsh::job-id previous) 3
        (cclsh::job-status previous) ':running
        (cclsh::job-touched previous) 10)
  (let* ((cclsh::*jobs* (list current previous))
         (result
           (check-completion
            "check-completion-declarative fast --job %"))
         (current-index (position "%+" (second result) :test #'string=)))
    (check-equal "job arguments complete the current selector"
                 t
                 (not (null current-index)))
    (check-equal "job arguments complete the previous selector"
                 t
                 (check-completion-candidate-p "%-" result))
    (check-equal "job arguments complete numeric selectors"
                 t
                 (and (check-completion-candidate-p "%7" result)
                      (check-completion-candidate-p "%3" result)))
    (check-equal "job completion describes status and command"
                 t
                 (and current-index
                      (not (null
                            (search "sleep current"
                                    (nth current-index
                                         (third result)))))))))

(let ((package (make-package "CCLSH CHECK COMPLETION PACKAGE" :use nil)))
  (unwind-protect
      (let ((result
              (check-completion
               "check-completion-declarative fast --package-name cclsh\\ check")))
        (check-equal "package candidates escape shell word separators"
                     t
                     (check-completion-candidate-p
                      "cclsh\\ check\\ completion\\ package" result)))
    (delete-package package)))

(let* ((root
         (format nil "/tmp/cclsh-check-completion-~d/"
                 (ccl:external-call "getpid" :int)))
       (file           (concatenate 'string root "sample target.txt"))
       (backslash-file (concatenate 'string root "back\\slash.txt"))
       (directory      (concatenate 'string root "sample-dir/"))
       (literal-file   (concatenate 'string root "~/literal target.txt"))
       (glob-first     (concatenate 'string root "glob-one"))
       (glob-second    (concatenate 'string root "glob-two"))
       (old-directory  (ccl:current-directory))
       (old-defaults   *default-pathname-defaults*)
       (old-home       (cclsh:getenv "HOME"))
       (glob-variable  "CCLSH_COMPLETION_AUDIT_GLOB")
       (old-glob       (cclsh:getenv glob-variable)))
  (unwind-protect
      (progn
        (check-write-utf8-file file "completion")
        ;; CCL pathname syntax treats a backslash as an escape. Create this
        ;; POSIX filename through an argv boundary so the byte remains literal.
        (uiop:run-program (list "/usr/bin/touch" backslash-file))
        (check-write-utf8-file literal-file "literal tilde")
        (check-write-utf8-file glob-first "first")
        (check-write-utf8-file glob-second "second")
        (ensure-directories-exist (concatenate 'string directory ".keep"))
        (setf (ccl:current-directory) root
              *default-pathname-defaults* (pathname root))
        (cclsh:setenv "HOME" (string-right-trim "/" root))
        (cclsh:setenv glob-variable "glob-*")
        (let* ((long-line
                 "check-completion-declarative fast --output=sample")
               (long-result (check-completion long-line)))
          (check-equal "long equals values replace only text after equals"
                       (1+ (position #\= long-line))
                       (first long-result))
          (check-equal "pathname arguments complete escaped files"
                       t
                       (check-completion-candidate-p
                        "sample\\ target.txt" long-result)))
        (let ((backslash-result
                (check-completion
                 "check-completion-declarative fast --output=back\\\\sl")))
          (check-equal "declared paths preserve an escaped literal backslash"
                       t
                       (check-completion-candidate-p
                        "back\\\\slash.txt" backslash-result)))
        (let ((tilde-result
                (check-completion
                 "check-completion-declarative fast --output=~/sample")))
          (check-equal "pathname completion preserves tilde spelling"
                       t
                       (check-completion-candidate-p
                        "~/sample\\ target.txt" tilde-result)))
        (let ((escaped-tilde-result
                (check-completion
                 "check-completion-declarative fast --output=\\~/literal"))
              (quoted-tilde-result
                (check-completion
                 "check-completion-declarative fast --output=\"~/literal")))
          (check-equal "escaped tilde completion remains relative"
                       t
                       (check-completion-candidate-p
                        "\\~/literal\\ target.txt" escaped-tilde-result))
          (check-equal "quoted tilde completion remains relative"
                       t
                       (check-completion-candidate-p
                        "\\~/literal\\ target.txt" quoted-tilde-result)))
        (let* ((short-line
                 "check-completion-declarative fast -qvosam")
               (short-result (check-completion short-line)))
          (check-equal "grouped shorts find the attached value replacement"
                       (search "sam" short-line :from-end t)
                       (first short-result))
          (check-equal "attached short values use semantic completion"
                       t
                       (check-completion-candidate-p
                        "sample\\ target.txt" short-result)))
        (let ((directory-result
                (check-completion
                 "check-completion-declarative fast --directory sample")))
          (check-equal "directory arguments include directories"
                       t
                       (check-completion-candidate-p
                        "sample-dir/" directory-result))
          (check-equal "directory arguments exclude ordinary files"
                       nil
                       (member "sample\\ target.txt"
                               (second directory-result)
                               :test #'string=)))
        (let ((legacy-result
                (check-completion "check-completion-legacy sample")))
          (check-equal "legacy commands retain generic file completion"
                       t
                       (check-completion-candidate-p
                        "sample\\ target.txt" legacy-result)))
        (let ((external-result (check-completion "cat sample")))
          (check-equal "external commands retain generic file completion"
                       t
                       (check-completion-candidate-p
                        "sample\\ target.txt" external-result)))
        (let ((glob-result
                (check-completion
                 (format nil
                         "check-completion-declarative $~a "
                         glob-variable))))
          (check-equal "a variable-introduced glob suppresses positional guesses"
                       nil
                       (second glob-result))))
    (ignore-errors
      (setf (ccl:current-directory) old-directory
            *default-pathname-defaults* old-defaults))
    (if old-home
        (cclsh:setenv "HOME" old-home)
        (cclsh::unsetenv "HOME"))
    (if old-glob
        (cclsh:setenv glob-variable old-glob)
        (cclsh::unsetenv glob-variable))
    (ignore-errors
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore))))

(setf *check-completion-side-effect* nil)
(let ((result
        (check-completion
         "check-completion-declarative (progn (setf *check-completion-side-effect* t) \"fast\") ")))
  (check-equal "argument completion never evaluates Lisp substitutions"
               nil
               *check-completion-side-effect*)
  (check-equal "unknown substitution argv size suppresses semantic guesses"
               nil
               (second result)))




;;;; -- Definition editing --

(multiple-value-bind (source pathname line)
    (cclsh::definition-edit--source #'check-edit-fixture)
  (declare (ignore pathname line))
  (check-equal "edit source note contains the fixture definition"
               t
               (not (null (search "(defun check-edit-fixture" source
                                  :test #'char-equal)))))

(check-equal "edit source line counts from one"
             1
             (cclsh::definition-edit--source-line
              (truename "source/builtins.lisp")
              0))

(let ((old-visual (cclsh:getenv "VISUAL"))
      (old-editor (cclsh:getenv "EDITOR")))
  (unwind-protect
       (progn
         (cclsh:setenv "VISUAL" "visual-editor --wait")
         (cclsh:setenv "EDITOR" "ignored-editor")
         (check-equal "edit prefers VISUAL and parses its arguments"
                      '("visual-editor" "--wait")
                      (cclsh::definition-edit--editor-words)))
    (if old-visual
        (cclsh:setenv "VISUAL" old-visual)
        (cclsh::unsetenv "VISUAL"))
    (if old-editor
        (cclsh:setenv "EDITOR" old-editor)
        (cclsh::unsetenv "EDITOR"))))

(let ((old-function (fdefinition 'check-edit-fixture)))
  (unwind-protect
       (let ((cclsh::*definition-editor-runner*
               (lambda (words pathname)
                 (declare (ignore words))
                 (let* ((text (uiop:read-file-string pathname))
                        (old "(+ value 1)")
                        (position (search old text)))
                   (unless position
                     (error "edit fixture body not found"))
                   (with-open-file (stream pathname
                                           :direction ':output
                                           :if-exists ':supersede
                                           :external-format ':utf-8)
                     (write-string text stream :end position)
                     (write-string "(+ value 10)" stream)
                     (write-string text stream
                                   :start (+ position (length old)))))
                 0)))
         (let* ((output
                  (with-output-to-string (stream)
                    (let ((*standard-output* stream))
                      (check-equal "edit returns success after redefining"
                                   0
                                   (cclsh:edit #'check-edit-fixture)))))
                (plain (cl-colorist:strip-ansi output)))
           (check-equal "edit renders the saved text through Colordiff"
                        t
                        (not (null (search "(+ value 10)" plain))))
           (check-equal "edit installs the saved definition in the image"
                        11
                        (check-edit-fixture 1))
           (multiple-value-bind (source pathname line)
               (cclsh::definition-edit--source #'check-edit-fixture)
             (declare (ignore pathname line))
             (check-equal "edit retains the new source for another edit"
                          t
                          (not (null (search "(+ value 10)" source)))))))
    (setf (fdefinition 'check-edit-fixture) old-function)))

(let ((cclsh::*definition-editor-runner*
        (lambda (words pathname)
          (declare (ignore words pathname))
          17)))
  (check-equal "edit returns a failing editor status without diffing"
               17
               (cclsh:edit #'check-edit-fixture)))
;;;; -- Result --

(cond (*check-failures*
       (format *error-output* "~d regression check~:p failed:~%"
               (length *check-failures*))
       (dolist (failure (nreverse *check-failures*))
         (format *error-output* "  ~a~%" failure))
       (ccl:quit 1))
      (t
       (format t "All cclsh regression checks passed.~%")
       (ccl:quit 0)))
