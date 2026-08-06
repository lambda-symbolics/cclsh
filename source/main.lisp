;;;; -- Main loop --
;;;
;;; Startup, the read loop with Lisp continuation lines, and the entry
;;; points for a REPL session and the saved application. The saved
;;; application is written to survive login shell duty: plain -c is safe
;;; for ssh, scp, rsync and git, -lc provides configured one-shots, unknown
;;; flags are ignored instead of refusing to start, a broken startup file or
;;; history never prevents a prompt, and CCLSH_SAFE=1 skips user state.

(in-package #:cclsh)

(defparameter *cclsh-version* "1.1.0"
  "The cclsh version reported by --version.")

(defvar *cclsh-build-commit* nil
  "Git commit the running binary was built from, stamped into the
   image by scripts/build.lisp. NIL in plain REPL sessions.")

(defvar *cclsh-build-clinedi-commit* nil
  "Clinedi commit included in the running binary, stamped into the
   image by scripts/build.lisp. NIL in plain REPL sessions.")

(defvar *cclsh-build-cl-colorist-commit* nil
  "cl-colorist commit included in the running binary, stamped into the
   image by scripts/build.lisp. NIL in plain REPL sessions.")

(defun terminal--encoding-leaves (&rest roots)
  "Return the distinct base streams below composite stream ROOTS."
  (let ((pending (copy-list roots))
        (seen nil)
        (leaves nil))
    (loop while pending
          for stream = (pop pending)
          unless (member stream seen :test #'eq)
            do (push stream seen)
               (cond ((typep stream 'synonym-stream)
                      (push (symbol-value (synonym-stream-symbol stream))
                            pending))
                     ((typep stream 'two-way-stream)
                      (push (two-way-stream-input-stream stream) pending)
                      (push (two-way-stream-output-stream stream) pending))
                     ((typep stream 'echo-stream)
                      (push (echo-stream-input-stream stream) pending)
                      (push (echo-stream-output-stream stream) pending))
                     ((typep stream 'broadcast-stream)
                      (setf pending
                            (append (broadcast-stream-streams stream)
                                    pending)))
                     ((typep stream 'concatenated-stream)
                      (setf pending
                            (append (concatenated-stream-streams stream)
                                    pending)))
                     (t
                      (push stream leaves))))
    (nreverse leaves)))

(defun terminal-encoding-setup ()
  "Make UTF-8 the default and switch every terminal stream to it."
  (setf ccl:*default-file-character-encoding* ':utf-8
        ccl:*default-external-format*
        '(:character-encoding :utf-8 :line-termination :unix)
        ccl:*terminal-character-encoding-name* ':utf-8)
  (let ((format (make-external-format :character-encoding ':utf-8
                                      :line-termination   ':unix)))
    (dolist (stream (terminal--encoding-leaves
                     *terminal-io* *standard-input*
                     *standard-output* *error-output*))
      (setf (stream-external-format stream) format)))
  (values))

(defun environment-setup ()
  "Prepare process environment variables for the shell and its children."
  (setenv "PWD" (directory-namestring-clean (current-directory)))
  (values))

(defun startup-file ()
  "Return the path of the user's startup file."
  (concatenate 'string (config-directory) "startup.lisp"))

(defun startup-load ()
  "Load the user's startup.lisp when present. Errors are reported and
   otherwise ignored so a broken startup file never bricks the shell."
  (let ((file (startup-file)))
    (when (probe-file file)
      (handler-case
          (load file :verbose nil :external-format ':utf-8)
        (serious-condition (condition)
          (dispatch-report-error condition)))))
  (values))

(defun shell-safe-mode-p ()
  "True when CCLSH_SAFE is set, which skips startup.lisp and history."
  (let ((value (getenv "CCLSH_SAFE")))
    (and value (plusp (length value)) t)))


;;; Reading complete inputs

(defun shell-read-interactive (duration-milliseconds)
  "Read one complete input with the line editor, following unfinished
   Lisp forms onto continuation lines. Returns (values line kind)."
  (multiple-value-bind (rows columns)
      (terminal-size)
    (declare (ignore rows))
    (terminal-fresh-line)
    (let ((prompt (prompt-render *last-status* duration-milliseconds columns)))
      (multiple-value-bind (line kind)
          (edit-line prompt)
        (if (not (eq kind ':line))
            (values line kind)
            (let ((accumulated line))
              (loop while (input-line-open-p accumulated)
                    do (multiple-value-bind (continuation continuation-kind)
                           (edit-line (ansi-colorize "... " ':bright-black))
                         (unless (eq continuation-kind ':line)
                           (return-from shell-read-interactive
                             (values nil ':abort)))
                         (setf accumulated
                               (input-line-join accumulated continuation))))
              (values accumulated ':line)))))))

(defun shell-read-plain ()
  "Read one complete input without the editor, for piped input.
   Returns (values line kind)."
  (let ((line (read-line *standard-input* nil nil)))
    (if (null line)
        (values nil ':eof)
        (let ((accumulated line))
          (loop while (input-line-open-p accumulated)
                do (let ((continuation (read-line *standard-input* nil nil)))
                     (when (null continuation)
                       (return))
                     (setf accumulated
                           (input-line-join accumulated continuation))))
          (values accumulated ':line)))))


;;; Entry points

(defun main ()
  "Run the shell until exit or end of input. Iteration errors are
   reported and survived; only a long unbroken run of failures makes
   the shell give up, so a login session is never lost to one bad
   prompt render or a flaky read. Piped input is a stateless scripting
   mode and therefore skips startup.lisp and history."
  (terminal-encoding-setup)
  (if *terminal-control-signals-active*
      (shell--main)
      (with-terminal-control-signals
        (shell--main))))

(defun shell--main ()
  "Run the initialized shell loop for MAIN."
  (terminal-shell-attributes-save)
  (environment-setup)
  (quicklisp-packaged-setup)
  (let ((safe        (shell-safe-mode-p))
        (interactive (terminal-tty-p)))
    (when (and interactive (not safe))
      (quicklisp-packaged-user-setup))
    (when (and interactive (not safe))
      (history-load))
    (let ((*package* (find-package '#:cclsh-user))
          (duration  0)
          (failures  0))
      (environment-package-sync)
      (when (and interactive (not safe))
        (startup-load))
      (loop
        (catch 'cclsh-toplevel
          (handler-case
              (let ((*break-hook* (lambda (&rest arguments)
                                    (declare (ignore arguments))
                                    (throw 'cclsh-toplevel nil))))
                (multiple-value-bind (line kind)
                    (if interactive
                        (progn
                          (jobs-notify)
                          (shell-read-interactive duration))
                        (shell-read-plain))
                  (ecase kind
                    (:eof
                     (let ((*jobs-exit-confirmed*
                             (shiftf *jobs-exit-warned* nil)))
                       (if (jobs-exit-blocked-p)
                           (setf *last-status* 1)
                           (shell-quit *last-status*))))
                    (:abort
                     (setf *last-status* 130))
                    (:line
                     (when interactive
                       (history-append line))
                     (let ((started (get-internal-real-time)))
                       (dispatch-line line)
                       (setf duration
                             (round (* 1000 (- (get-internal-real-time)
                                               started))
                                    internal-time-units-per-second))))))
                (setf failures 0))
            (serious-condition (condition)
              (terminal-restore)
              (dispatch-report-error condition)
              (incf failures)
              (when (>= failures 25)
                (format *error-output*
                        "cclsh: too many consecutive errors, giving up~%")
                (shell-quit 70)))))))))


;;; Command line arguments

(defun shell--execute-command-string (command &key configured-p)
  "Run COMMAND as one shell input and exit with its status.
   Plain -c skips startup.lisp and history so user state cannot break
   remote access. When CONFIGURED-P is true, load startup.lisp first;
   CCLSH_SAFE still suppresses it. Command mode never loads history."
  (terminal-encoding-setup)
  (terminal-shell-attributes-save)
  (environment-setup)
  (quicklisp-packaged-setup)
  (let ((*package* (find-package '#:cclsh-user)))
    (environment-package-sync)
    (when (and configured-p (not (shell-safe-mode-p)))
      (quicklisp-packaged-user-setup)
      (startup-load))
    (dispatch-line command))
  (shell-quit *last-status*))

(defun shell--run-script (path arguments)
  "Run the file at PATH as a cclsh script and exit with the last
   status. Like -c, scripts load no user state; this is also the
   shebang entry point since the kernel passes the script path as the
   first argument. Bind *ARGV* to PATH followed by ARGUMENTS."
  (terminal-encoding-setup)
  (terminal-shell-attributes-save)
  (environment-setup)
  (quicklisp-packaged-setup)
  (let ((*package* (find-package '#:cclsh-user))
        (*argv*    (list* path (copy-list arguments))))
    (environment-package-sync)
    (handler-case
        (with-open-file (stream path
                                :direction :input
                                :external-format ':utf-8)
          (let ((*standard-input* stream))
            (loop
              (multiple-value-bind (line kind)
                  (shell-read-plain)
                (when (eq kind ':eof)
                  (return))
                (dispatch-line line)))))
      (error (condition)
        (dispatch-report-error condition)
        (shell-quit 127))))
  (shell-quit *last-status*))

(defun shell--short-option-group (argument)
  "Return the option characters in a single-dash ARGUMENT, or NIL.
   Long options, a lone dash and non-option arguments are not groups."
  (when (and (> (length argument) 1)
             (char= (char argument 0) #\-)
             (not (char= (char argument 1) #\-)))
    (subseq argument 1)))

(defun shell--argument-plan (arguments)
  "Decode ARGUMENTS without performing an action.
   Return ACTION, OPERAND, CONFIGURED-P and SCRIPT-ARGUMENTS. ACTION is
   :MAIN, :COMMAND, :MISSING-COMMAND, :MANUAL, :SCRIPT, :VERSION or :HELP.
   For :MANUAL, OPERAND is the list of requested sections or commands.
   Lowercase c selects command mode anywhere in a single-dash group.
   Lowercase l or i in that group, or an earlier group, requests
   startup.lisp for command mode. Other option letters and unknown long
   options are ignored. A double dash ends option parsing."
  (loop with remaining = arguments
        with configured-p = nil
        with options-p = t
        while remaining
        do (let* ((argument (pop remaining))
                  (options  (and options-p
                                 (shell--short-option-group argument))))
             (cond ((and options-p (string= argument "--version"))
                    (return (values ':version nil configured-p nil)))
                   ((and options-p (string= argument "--help"))
                    (return (values ':help nil configured-p nil)))
                   ((and options-p (string= argument "--"))
                    (setf options-p nil))
                   ((and options-p (string= argument "help"))
                    (return
                      (values ':manual remaining configured-p nil)))
                   ((and options-p options)
                    (when (or (find #\l options) (find #\i options))
                      (setf configured-p t))
                    (when (find #\c options)
                      (return
                        (if remaining
                            (values ':command (pop remaining)
                                    configured-p nil)
                            (values ':missing-command nil
                                    configured-p nil)))))
                   ((and options-p
                         (plusp (length argument))
                         (char= (char argument 0) #\-))
                    nil)
                   (t
                    (return
                      (values ':script argument configured-p remaining)))))
        finally (return (values ':main nil configured-p nil))))

(defun shell--process-arguments (arguments)
  "Handle command line ARGUMENTS. Returns only when the shell should
   start its normal read loop; command strings, manual requests, --version,
   --help and script files exit the process themselves. Short flags may be
   combined in any order. Unknown flags are deliberately ignored so an
   exotic login invocation cannot lock anyone out."
  (multiple-value-bind (action operand configured-p script-arguments)
      (shell--argument-plan arguments)
    (ecase action
      (:main
       (values))
      (:command
       (shell--execute-command-string operand :configured-p configured-p))
      (:missing-command
       (format *error-output* "cclsh: -c requires an argument~%")
       (shell-quit 2))
      (:manual
       (terminal-encoding-setup)
       (let ((*package* (find-package '#:cclsh)))
         (shell-quit (apply #'help operand))))
      (:script
       (shell--run-script operand script-arguments))
      (:version
       (format t "cclsh ~a~@[ (~a)~]~@[ (clinedi ~a)~]~
                  ~@[ (cl-colorist ~a)~] (~a ~a)~%"
               *cclsh-version*
               *cclsh-build-commit*
               *cclsh-build-clinedi-commit*
               *cclsh-build-cl-colorist-commit*
               (lisp-implementation-type)
               (lisp-implementation-version))
       (shell-quit 0))
      (:help
       (format t "usage: cclsh [-il] [-c command] [--] ~
                  [script [argument...]] ~
                  [--version] [--help]~%~
                  cclsh help [section-or-command...]~%~
                  short flags may be combined; -l/-i with -c load ~
                  startup.lisp~%~
                  use -- help to run a script named help~%")
       (shell-quit 0)))))

(defun shell--executable-path ()
  "Resolved path of the running executable, or NIL."
  (ignore-errors
    (namestring (truename (ccl::kernel-path)))))

(defun shell--path-references-executable-p (path executable-path)
  "True when PATH names EXECUTABLE-PATH, including through symlinks."
  (and path
       executable-path
       (or (string= path executable-path)
           (ignore-errors
             (equal (truename path) (truename executable-path))))))

(defun shell--invocation-path
    (&key
       (arguments *command-line-argument-list*)
       (environment-shell (getenv "SHELL"))
       (executable-path (shell--executable-path)))
  "Stable shell path from argv[0], preserving an installed symlink.
   Traditional login programs replace argv[0] with a dash-prefixed name;
   in that case use an absolute SHELL only when it names this executable."
  (let ((argument (first arguments)))
    (cond ((and argument
                (plusp (length argument))
                (char= (char argument 0) #\/)
                (shell--path-references-executable-p
                 argument executable-path))
           argument)
          ((and argument
                (plusp (length argument))
                (char= (char argument 0) #\-)
                environment-shell
                (plusp (length environment-shell))
                (char= (char environment-shell 0) #\/)
                (shell--path-references-executable-p
                 environment-shell executable-path))
           environment-shell)
          (t
           nil))))

(defun shell-toplevel ()
  "Entry point for the saved cclsh application. Sets SHELL to the
   stable invocation path, or the resolved running binary as fallback,
   so $SHELL callers land back in cclsh. A REPL session through MAIN
   leaves SHELL alone."
  (let ((worker-descriptor (getenv +prewarm-worker-fd-variable+))
        (arguments (rest *command-line-argument-list*)))
    (if (and worker-descriptor
             (equal arguments (list +prewarm-worker-argument+)))
        (progn
          (handler-case
              (prewarm-worker-lease worker-descriptor)
            (serious-condition (condition)
              (declare (ignore condition))
              (prewarm--hard-exit)))
          (setf ccl:*command-line-argument-list*
                (list (first ccl:*command-line-argument-list*))
                ccl:*unprocessed-command-line-arguments* nil)
          (with-terminal-control-signals
            (handler-case
                (main)
              (serious-condition (condition)
                (dispatch-report-error condition)
                (shell-quit 70)))))
        (with-terminal-control-signals
          (handler-case
              (progn
                (let ((executable
                        (or (shell--invocation-path)
                            (shell--executable-path))))
                  (when executable
                    (setenv "SHELL" executable)))
                (shell--process-arguments arguments)
                (main))
            (serious-condition (condition)
              (dispatch-report-error condition)
              (shell-quit 70)))))))
