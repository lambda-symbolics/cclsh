;;;; -- Process and terminal integration checks --
;;;
;;; This suite exercises the saved executable. Run SCRIPTS/BUILD before
;;; SCRIPTS/INTEGRATION-CHECK so the image contains the source under test.

(require :asdf)

(defvar *integration-failures* nil
  "Descriptions of failed integration checks.")

(defvar *integration-file-counter* 0
  "Counter used to give subprocess output files unique names.")

(defparameter *integration-directory*
  (format nil "/tmp/cclsh-integration-~d-~d/"
          (ccl:external-call "getpid" :int)
          (get-universal-time))
  "Private temporary directory for this integration run.")

(defparameter *integration-bin-directory*
  (concatenate 'string *integration-directory* "bin/"))

(defparameter *integration-binary*
  (namestring (truename "cclsh")))


;;;; -- Small assertions --

(defun integration-fail (control &rest arguments)
  "Signal an integration failure formatted by CONTROL and ARGUMENTS."
  (error "~?" control arguments))

(defun integration-ensure (value control &rest arguments)
  "Require VALUE to be true, otherwise fail with CONTROL and ARGUMENTS."
  (unless value
    (apply #'integration-fail control arguments))
  value)

(defun integration-test (name function)
  "Run FUNCTION as test NAME, record its failure and continue."
  (handler-case
      (progn
        (funcall function)
        (format t "[ OK ] ~a~%" name))
    (serious-condition (condition)
      (push (format nil "~a: ~a" name condition) *integration-failures*)
      (format *error-output* "[FAIL] ~a: ~a~%" name condition)))
  (finish-output)
  (finish-output *error-output*))

(defun integration-contains-p (needle haystack)
  "True when HAYSTACK contains NEEDLE."
  (not (null (search needle haystack))))

(defun integration-skip-ansi (text start)
  "Return the first index after the ANSI sequence at START in TEXT."
  (let ((length (length text)))
    (if (>= (1+ start) length)
        length
        (case (char text (1+ start))
          (#\[
           (or (position-if
                (lambda (char)
                  (let ((code (char-code char)))
                    (<= #x40 code #x7e)))
                text :start (+ start 2))
               (1- length)))
          (#\]
           (let ((index (+ start 2)))
             (loop while (< index length)
                   for char = (char text index)
                   do (cond ((char= char (code-char 7))
                             (return index))
                            ((and (char= char (code-char 27))
                                  (< (1+ index) length)
                                  (char= (char text (1+ index)) #\\))
                             (return (1+ index))))
                      (incf index)
                   finally (return (1- length)))))
          (t
           (1+ start))))))

(defun integration-clean-text (text)
  "Remove terminal presentation sequences and carriage returns from TEXT."
  (with-output-to-string (stream)
    (loop with index = 0
          while (< index (length text))
          for char = (char text index)
          do (cond ((char= char (code-char 27))
                    (setf index (integration-skip-ansi text index)))
                   ((not (char= char #\return))
                    (write-char char stream)))
             (incf index))))

(defun integration-tail (text &optional (limit 1200))
  "Return at most the last LIMIT characters of cleaned TEXT."
  (let* ((clean (integration-clean-text text))
         (start (max 0 (- (length clean) limit))))
    (subseq clean start)))

(defun integration-integer-after (marker text)
  "Return the last decimal integer printed directly after MARKER in TEXT."
  (let ((clean (integration-clean-text text))
        (start 0)
        (found nil))
    (loop for marker-start = (search marker clean :start2 start)
          while marker-start
          for number-start = (+ marker-start (length marker))
          do (when (and (< number-start (length clean))
                        (digit-char-p (char clean number-start)))
               (multiple-value-bind (number end)
                   (parse-integer clean :start number-start :junk-allowed t)
                 (declare (ignore end))
                 (setf found number)))
             (setf start (1+ marker-start)))
    found))


;;;; -- Files and test programs --

(defun integration-path (name)
  "Return NAME below the integration temporary directory."
  (concatenate 'string *integration-directory* name))

(defun integration-output-path (kind)
  "Return a fresh temporary output path tagged with KIND."
  (integration-path
   (format nil "result-~d.~a" (incf *integration-file-counter*) kind)))

(defun integration-read-file (path)
  "Read the UTF-8 text file at PATH completely."
  (with-open-file (stream path
                          :direction ':input
                          :external-format ':utf-8)
    (let ((text (make-string (file-length stream))))
      (let ((end (read-sequence text stream)))
        (if (= end (length text)) text (subseq text 0 end))))))

(defun integration-write-file (path contents)
  "Write CONTENTS as UTF-8 text to PATH."
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create
                          :external-format ':utf-8)
    (write-string contents stream))
  path)

(defun integration-write-octets (path octets)
  "Write OCTETS to PATH without character encoding."
  (with-open-file (stream path
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create
                          :element-type '(unsigned-byte 8))
    (write-sequence octets stream))
  path)

(defun integration-read-octets (path)
  "Read PATH as an octet vector."
  (with-open-file (stream path
                          :direction ':input
                          :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream)
                              :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

(defun integration-write-program (name contents)
  "Create executable test program NAME with CONTENTS and return its path."
  (let ((path (concatenate 'string *integration-bin-directory* name)))
    (integration-write-file path contents)
    (let ((process (ccl:run-program "/usr/bin/chmod" (list "755" path)
                                    :input nil :output nil :error nil
                                    :wait t)))
      (multiple-value-bind (state code)
          (ccl:external-process-status process)
        (integration-ensure (and (eq state ':exited) (zerop code))
                            "chmod failed for ~a" path)))
    path))

(defun integration-install-programs ()
  "Install deterministic prompt and child programs used by the suite."
  (ensure-directories-exist
   (concatenate 'string *integration-bin-directory* ".keep"))
  (integration-write-program
   "prompt-renderer"
   "#!/bin/sh
file=$CCLSH_TEST_ROOT/prompt-count
n=0
if test -r \"$file\"; then read n < \"$file\"; fi
n=$((n + 1))
printf '%s\\n' \"$n\" > \"$file\"
printf '%s\\n' \"\${CCLSH_PACKAGE-}\" > \"$CCLSH_TEST_ROOT/prompt-package\"
printf '你好 🐈 λ __CCLSH_PROMPT_%s__ ' \"$n\"
")
  (integration-write-file
   (integration-path "cclsh/startup.lisp")
   "(in-package #:cclsh-user)

(defun integration--prompt-renderer
    (&key status duration-milliseconds columns job-count
     &allow-other-keys)
  \"Render the integration prompt through its deterministic test program.\"
  (declare (ignore status duration-milliseconds columns job-count))
  (handler-case
      (let* ((output (make-string-output-stream))
             (process
               (ccl:run-program \"prompt-renderer\" nil
                                :input nil
                                :output output
                                :error nil
                                :wait t
                                :external-format ':utf-8)))
        (multiple-value-bind (state code)
            (ccl:external-process-status process)
          (let ((prompt (get-output-stream-string output)))
            (and (eq state ':exited)
                 (zerop code)
                 (plusp (length prompt))
                 prompt))))
    (error () nil)))

(setf *prompt-function* 'integration--prompt-renderer)
")
  (integration-write-program
   "pgid-left"
   "#!/bin/sh
ps -o pgid= -p $$ | tr -d ' '
")
  (integration-write-program
   "pgid-right"
   "#!/bin/sh
IFS= read -r left
right=$(ps -o pgid= -p $$ | tr -d ' ')
printf '__PGIDS__%s:%s\\n' \"$left\" \"$right\"
")
  (integration-write-program
   "library-foreground"
   "#!/bin/sh
group=$(ps -o pgid= -p $$ | tr -d ' ')
foreground=$(ps -o tpgid= -p $$ | tr -d ' ')
if test \"$group\" = \"$foreground\"; then
    printf '__LIBRARY_CHILD_FOREGROUND__\\n'
else
    printf '__LIBRARY_CHILD_BACKGROUND__%s:%s\\n' \"$group\" \"$foreground\"
    exit 9
fi
")
  (integration-write-program
   "library-stop"
   "#!/bin/sh
kill -STOP $$
printf '__LIBRARY_STOP_RESUMED__\\n'
")
  (integration-write-program
   "longproducer"
   "#!/bin/sh
trap 'printf \"__LONG_CONTINUED__\\n\"' CONT
group=$(ps -o pgid= -p $$ | tr -d ' ')
foreground()
{
    test \"$(ps -o tpgid= -p $$ | tr -d ' ')\" = \"$group\"
}
while ! foreground; do sleep 0.01; done
printf '__LONG_STARTED__\\n'
while :; do
    sleep 0.2
    if foreground; then printf '__LONG_TICK__\\n'; fi
done
")
  (integration-write-program
   "run-one-line"
   "#!/bin/sh
printf '__RUN_READER_READY__\\n' >&2
IFS= read -r line || exit 7
printf '__RUN_READER_VALUE__%s\\n' \"$line\"
")
  (integration-write-program
   "run-line-counter"
   "#!/bin/sh
count=$(wc -l)
printf '__RUN_READER_COUNT__%s\\n' \"$count\"
")
  (integration-write-program
   "modecheck"
   "#!/bin/sh
if stty -a | tr ';' '\\n' | grep -Eq '(^|[[:space:]])-echo([[:space:]]|$)'; then
    printf '__ECHO_OFF__\\n'
else
    printf '__ECHO_ON__\\n'
fi
")
  (integration-write-program
   "set-noecho"
   "#!/bin/sh
stty -echo
")
  (integration-write-program
   "stop-noecho"
   "#!/bin/sh
trap 'exit 130' INT
stty -echo
printf '__NOECHO_SET__\\n'
kill -STOP $$
if stty -a | grep -Eq '(^|[[:space:];])-echo([[:space:];]|$)'; then
    printf '__RESUMED_NOECHO__\\n'
else
    printf '__RESUMED_ECHO__\\n'
fi
stty echo
printf '__MODE_RESUMED__\\n'
IFS= read -r _
printf '__INTERRUPT_READY__\\n'
IFS= read -r _
")
  (integration-write-program
   "record-sleeper"
   "#!/bin/sh
printf '%s\\n' $$ > \"$CCLSH_TEST_ROOT/spawn.pid\"
exec sleep 30
")
  (integration-write-program
   "exit-observer"
   "#!/bin/sh
name=$1
mode=${2:-running}
prefix=$CCLSH_TEST_ROOT/exit-$name
trap 'printf \"HUP\\n\" >> \"$prefix.events\"; exit 0' HUP
printf '%s\\n' $$ > \"$prefix.pid\"
if test \"$mode\" = stopped; then
    kill -STOP $$
fi
while :; do sleep 1; done
")
  (integration-write-program
   "bounded-session"
   "#!/bin/sh
sidfile=$1
seconds=$2
shift 2
printf '%s\\n' $$ > \"$sidfile\"
exec /usr/bin/timeout --signal=TERM --kill-after=1 \"$seconds\" \"$@\"
")
  (integration-write-program
   "run-in-directory"
   "#!/bin/sh
directory=$1
shift
cd \"$directory\" || exit 125
exec \"$@\"
")
  (integration-write-program
   "login-cclsh"
   "#!/usr/bin/env bash
exec -a -cclsh \"$CCLSH_TEST_BINARY\" \"$@\"
")
  (integration-write-program
   "--no-avx"
   "#!/bin/sh
printf '__RESERVED_COMMAND_OPERAND__\\n'
")
  (integration-write-program
   "pty-session"
   "#!/bin/sh
sidfile=$1
shift
printf '%s\\n' $$ > \"$sidfile\"
exec /usr/bin/script \"$@\"
")
  (integration-write-program
   "mark-and-sleep"
   "#!/bin/sh
: > \"$CCLSH_TEST_ROOT/side-effect\"
exec sleep 30
")
  (integration-write-program
   "utf8-žluťoučký"
   "#!/bin/sh
printf '%s\\n' \"$1\"
")
  (integration-write-program
   "zoxide"
   "#!/bin/sh
log=$CCLSH_TEST_ROOT/zoxide.log
command=$1
shift
{
    printf '%s' \"$command\"
    for argument do printf '|%s' \"$argument\"; done
    printf '\\n'
} >> \"$log\"
case \"$command\" in
    add)
        case \"$*\" in *add-fail*) exit 9;; esac
        ;;
    query)
        case \"$*\" in
            *fail*) exit 7;;
            *empty*) exit 0;;
            *--interactive*) cat \"$CCLSH_TEST_ROOT/zoxide-interactive-result\";;
            *) cat \"$CCLSH_TEST_ROOT/zoxide-result\";;
        esac
        ;;
    *)
        exit 2
        ;;
esac
")
  (integration-write-program
   "z"
   "#!/bin/sh
exit 0
")
  (integration-write-program
   "zi"
   "#!/bin/sh
exit 0
")
  ;; An executable without a recognized image or interpreter makes
  ;; POSIX-SPAWN fail after preceding children have been created.
  (integration-write-program "bad-program"
                             (format nil "not an executable image~%")))

(defun integration-environment ()
  "Environment overrides shared by direct and interactive test shells."
  (list (cons "PATH"
              (format nil "~a:/usr/bin:/bin" *integration-bin-directory*))
        (cons "HOME" *integration-directory*)
        (cons "XDG_CACHE_HOME" *integration-directory*)
        (cons "XDG_CONFIG_HOME" *integration-directory*)
        (cons "XDG_DATA_HOME" *integration-directory*)
        (cons "CCLSH_SAFE" "1")
        (cons "CCLSH_QUICKLISP_SETUP" "/definitely/missing/setup.lisp")
        (cons "CCLSH_TEST_ROOT"
              (string-right-trim "/" *integration-directory*))
        (cons "CCLSH_TEST_BINARY" *integration-binary*)
        (cons "LANG" "C")
        (cons "LC_ALL" "C")
        (cons "TERM" "xterm-256color")))

(defun integration-environment-replace (&rest replacements)
  "Return the integration environment with REPLACEMENTS applied.
   Each replacement is a NAME . VALUE pair."
  (append replacements
          (remove-if
           (lambda (entry)
             (assoc (car entry) replacements :test #'string=))
           (integration-environment))))


;;;; -- Bounded direct execution --

(defstruct direct-result
  "Captured result of one saved-image command."
  (status 0 :type integer)
  (output "" :type string)
  (error-output "" :type string))

(defun integration-signal-session (session-id signal)
  "Send SIGNAL to every surviving process in SESSION-ID."
  (ignore-errors
    (ccl:run-program "/usr/bin/pkill"
                     (list (format nil "-~a" signal)
                           "-s" (princ-to-string session-id))
                     :input nil :output nil :error nil :wait t))
  (values))

(defun integration-kill-session (session-id)
  "Kill every surviving process in SESSION-ID."
  (integration-signal-session session-id "KILL"))

(defun integration-process-status (process)
  "Return conventional integer status for completed PROCESS."
  (multiple-value-bind (state code)
      (ccl:external-process-status process)
    (case state
      (:exited (or code 1))
      (:signaled (+ 128 (or code 0)))
      (t 125))))

(defun integration-run-arguments (arguments
                                  &key
                                    (timeout 10)
                                    input
                                    (program *integration-binary*)
                                    directory
                                    (environment (integration-environment)))
  "Run PROGRAM with ARGUMENTS in a private, bounded session.
   PROGRAM defaults to saved cclsh. INPUT is NIL or a pathname suitable
   for CCL:RUN-PROGRAM. DIRECTORY optionally changes the child's working
   directory. ENVIRONMENT defaults to the isolated integration environment."
  (let ((output-path (integration-output-path "out"))
        (error-path  (integration-output-path "err"))
        (session-path (integration-output-path "sid"))
        (process nil)
        (session-id nil)
        (completed nil))
    (unwind-protect
        (progn
          (setf process
                (ccl:run-program
                 "/usr/bin/setsid"
                 (append
                  (list "--wait"
                        (concatenate 'string *integration-bin-directory*
                                     "bounded-session")
                        session-path
                        (princ-to-string timeout))
                  (when directory
                    (list (concatenate 'string *integration-bin-directory*
                                       "run-in-directory")
                          directory))
                  (list program)
                  arguments)
                 :input input
                 :output output-path
                 :if-output-exists ':supersede
                 :error error-path
                 :if-error-exists ':supersede
                 :env environment
                 :wait nil
                 :external-format ':utf-8))
          (setf completed
                (ccl:timed-wait-on-semaphore
                 (ccl::external-process-completed process)
                 (+ timeout 3)))
          (when (probe-file session-path)
            (setf session-id
                  (parse-integer
                   (string-trim '(#\space #\tab #\newline #\return)
                                (integration-read-file session-path)))))
          (unless completed
            (when session-id
              (integration-kill-session session-id))
            (ccl:timed-wait-on-semaphore
             (ccl::external-process-completed process) 2))
          (let ((status (if completed
                            (integration-process-status process)
                            124)))
            (when (and (= status 124) session-id)
              (integration-kill-session session-id))
            (make-direct-result
             :status status
             :output (if (probe-file output-path)
                         (integration-read-file output-path)
                         "")
             :error-output (if (probe-file error-path)
                               (integration-read-file error-path)
                               ""))))
      (when (and process (not completed) session-id)
        (integration-kill-session session-id))
      (ignore-errors (delete-file output-path))
      (ignore-errors (delete-file error-path))
      (ignore-errors (delete-file session-path)))))

(defun integration-run (command &key (timeout 10))
  "Run saved cclsh -c COMMAND in a private session with a hard timeout."
  (integration-run-arguments (list "-c" command) :timeout timeout))

(defun integration-require-success (result context)
  "Require RESULT to have status zero, reporting its captured tail."
  (integration-ensure
   (zerop (direct-result-status result))
   "~a exited ~d~%stdout: ~a~%stderr: ~a"
   context
   (direct-result-status result)
   (integration-tail (direct-result-output result))
   (integration-tail (direct-result-error-output result))))

(defun integration-require-script-arguments (result expected context)
  "Require RESULT to run a stateless script with EXPECTED as its *ARGV*."
  (integration-require-success result context)
  (integration-ensure
   (and (integration-contains-p "__SCRIPT_ONLY__"
                                (direct-result-output result))
        (integration-contains-p "__SCRIPT_STARTUP__absent__"
                                (direct-result-output result))
        (integration-contains-p (format nil "__SCRIPT_ARGV__~s__" expected)
                                (direct-result-output result))
        (integration-contains-p "__SCRIPT_COMMENT_STATUS__1__"
                                (direct-result-output result))
        (integration-contains-p
         (format nil "__SCRIPT_PIPE_ARGV__~s__" expected)
         (direct-result-output result))
        (integration-contains-p
         (format nil "__SCRIPT_CAPTURE_ARGV__~s__" expected)
         (direct-result-output result))
        (integration-contains-p "__SCRIPT_CAPTURE_STATUS__0__"
                                (direct-result-output result)))
   "~a did not preserve stateless script arguments: ~a"
   context
   (integration-tail (direct-result-output result))))


;;;; -- Direct pipeline checks --

(defun integration-check-library-pty ()
  "Require a Quickloaded pipeline to return control to its host CCL."
  (let* ((setup
           (or (let ((override (uiop:getenv "CCLSH_QUICKLISP_SETUP")))
                 (and override
                      (plusp (length override))
                      (probe-file override)))
               (probe-file
                (merge-pathnames "quicklisp/setup.lisp"
                                 (user-homedir-pathname)))
               (integration-fail "Quicklisp setup.lisp is unavailable")))
         (input (integration-path "library-host.input"))
         (clinedi-directory
           (let ((override (uiop:getenv "CCLSH_CLINEDI_SOURCE")))
             (if (and override (plusp (length override)))
                 (uiop:ensure-directory-pathname override)
                 (truename "../clinedi/"))))
         (clinedi-asd
           (truename (merge-pathnames "clinedi.asd" clinedi-directory)))
         (cl-colorist-directory
           (let ((override (uiop:getenv "CCLSH_CL_COLORIST_SOURCE")))
             (if (and override (plusp (length override)))
                 (uiop:ensure-directory-pathname override)
                 (truename "../cl-colorist/"))))
         (cl-colorist-asd
           (truename
            (merge-pathnames "cl-colorist.asd" cl-colorist-directory)))
         (cclsh-asd (truename "cclsh.asd"))
         (ccl-command (or (uiop:getenv "CCLSH_TEST_CCL") "ccl"))
         (ccl-image (uiop:getenv "CCLSH_TEST_CCL_IMAGE"))
         (ccl-command-line
           (format nil "~{~a~^ ~}"
                   (mapcar
                    #'integration-shell-quote
                    (append (list ccl-command)
                            (when (and ccl-image
                                       (plusp (length ccl-image)))
                              (list "-I" ccl-image))
                            (list "-n"))))))
    (integration-write-file
     input
     (format nil
             "(require :asdf)~%
              (ccl:%stack-block ((signals 128))~%
                (unless~%
                    (and (zerop (ccl:external-call~%
                                 \"sigemptyset\"~%
                                 :address signals :int))~%
                         (zerop (ccl:external-call~%
                                 \"sigaddset\"~%
                                 :address signals :int 22 :int))~%
                         (zerop (ccl:external-call~%
                                 \"pthread_sigmask\"~%
                                 :int 1~%
                                 :address signals~%
                                 :address (ccl:%null-ptr)~%
                                 :int)))~%
                  (error \"cannot establish the signal-mask precondition\")))~%
              (ccl:external-call~%
               \"signal\" :int 22 :address (ccl:%int-to-ptr 0) :address)~%
              (load ~s :verbose nil :external-format :utf-8)~%
              (asdf:load-asd ~s)~%
              (asdf:load-asd ~s)~%
              (asdf:load-asd ~s)~%
              (ql:quickload :cclsh :silent t)~%
              (in-package :cclsh-user)~%
              (flet ((sigttou-blocked-p ()~%
                       (ccl:%stack-block~%
                           ((signals cclsh::+terminal-sigset-size+))~%
                         (unless~%
                             (zerop~%
                              (ccl:external-call~%
                               \"pthread_sigmask\"~%
                               :int cclsh::+terminal-sig-block+~%
                               :address (ccl:%null-ptr)~%
                               :address signals~%
                               :int))~%
                           (error \"cannot inspect the signal mask\"))~%
                         (= 1 (ccl:external-call~%
                               \"sigismember\"~%
                               :address signals~%
                               :int cclsh::+sigttou+~%
                               :int))))~%
                      (sigttou-default-p ()~%
                        (let* ((default (ccl:%int-to-ptr 0))~%
                               (previous~%
                                 (ccl:external-call~%
                                  \"signal\"~%
                                  :int cclsh::+sigttou+~%
                                  :address default~%
                                  :address)))~%
                          (unwind-protect~%
                              (ccl:%ptr-eql previous default)~%
                            (ccl:external-call~%
                             \"signal\"~%
                             :int cclsh::+sigttou+~%
                             :address previous~%
                             :address)))))~%
                (let ((before (sigttou-blocked-p))~%
                      (stopped-status (pipe (library-stop)))~%
                      (resumed-status (fg))~%
                      (status (pipe (echo \"balls\") (rev)))~%
                      (foreground-status~%
                        (pipe (library-foreground) (cat))))~%
                  (multiple-value-bind (foreground code)~%
                      (cclsh::terminal-current-foreground)~%
                    (format t~%
                            \"__LIBRARY~~cPIPE__~~s:~~s:~~s:~~s:~~s:~~s:~~s:~~s:~~s__~~%\"~%
                            #\\_~%
                            (= stopped-status~%
                               (+ 128 cclsh::+sigtstp+))~%
                            (zerop resumed-status)~%
                            (zerop status)~%
                            (zerop foreground-status)~%
                            (and foreground~%
                                 (= foreground~%
                                    (cclsh::terminal-own-process-group)))~%
                            (zerop code)~%
                            (null cclsh::*terminal-control-signals-active*)~%
                            (and (not before)~%
                                 (eql before (sigttou-blocked-p)))~%
                            (sigttou-default-p)))))~%
              (ccl:quit 0)~%"
             (namestring setup)
             (namestring cl-colorist-asd)
             (namestring clinedi-asd)
             (namestring cclsh-asd)))
    (let* ((result
             (integration-run-arguments
              (list "-qefc" ccl-command-line "/dev/null")
              :program "/usr/bin/script"
              :input input
              :timeout 30))
           (output (direct-result-output result)))
      (integration-require-success result "Quickloaded CCLSH pipeline")
      (integration-ensure
       (integration-contains-p "sllab" output)
       "Quickloaded pipeline produced no reversed output: ~a"
       (integration-tail output))
      (integration-ensure
       (integration-contains-p "__LIBRARY_CHILD_FOREGROUND__" output)
       "Quickloaded child did not own the terminal: ~a"
       (integration-tail output))
      (integration-ensure
       (integration-contains-p "__LIBRARY_STOP_RESUMED__" output)
       "Quickloaded stopped job did not resume: ~a"
       (integration-tail output))
      (integration-ensure
       (integration-contains-p
        "__LIBRARY_PIPE__T:T:T:T:T:T:T:T:T__" output)
       "host CCL did not reclaim its terminal cleanly: ~a"
       (integration-tail output)))))

(defun integration-check-image-startup ()
  "Start the saved image repeatedly to catch resume-path failures."
  (loop for attempt from 1 to 100
        for result = (integration-run "exit 0" :timeout 2)
        do (integration-require-success
            result (format nil "saved-image startup ~d" attempt))))

(defun integration-check-lisp-glob ()
  "Require the saved image to export GLOB and splice its ordinary list."
  (let* ((directory (integration-path "lisp-glob/"))
         (alpha     (concatenate 'string directory "alpha.screenshot"))
         (space     (concatenate 'string directory
                                 "beta space.screenshot"))
         (pattern   (concatenate 'string directory "*.screenshot"))
         (expected  (format nil "<~a>~%<~a>" alpha space))
         (form
           (format nil
                   "(progn
                      (let ((matches (glob ~s)))
                        (multiple-value-bind (text status)
                            (capture (printf \"<%s>\\\\n\" matches))
                          (format t
                                  \"__GLOB_CAPTURE__~~s__GLOB_STATUS__~~d__~~%\"
                                  text status)))
                      (multiple-value-bind (text status)
                          (capture (printf \"%s\" ~s))
                        (format t
                                \"__LITERAL_CAPTURE__~~s__LITERAL_STATUS__~~d__~~%\"
                                text status))
                      (values))"
                   pattern pattern)))
    (integration-write-file alpha "alpha")
    (integration-write-file space "space")
    (let* ((result (integration-run form))
           (output (direct-result-output result)))
      (integration-require-success result "saved-image Lisp glob")
      (integration-ensure
       (integration-contains-p
        (format nil "__GLOB_CAPTURE__~s__GLOB_STATUS__0__" expected)
        output)
       "saved-image glob did not splice sorted matches: ~a"
       (integration-tail output))
      (integration-ensure
       (integration-contains-p
        (format nil "__LITERAL_CAPTURE__~s__LITERAL_STATUS__0__" pattern)
        output)
       "ordinary saved-image string globbed implicitly: ~a"
       (integration-tail output)))))

(defun integration-check-command-line-modes ()
  "Require combined flags and stateless scripting modes to honor user state."
  (let* ((xdg              (integration-path "argument-modes-config/"))
         (config           (concatenate 'string xdg "cclsh/"))
         (startup          (concatenate 'string config "startup.lisp"))
         (history          (concatenate 'string config "history"))
         (script           (integration-path
                            "argument mode 猫 script.sh.lisp"))
         (help-script-name "help")
         (help-script      (integration-path help-script-name))
         (dash-script-name "--no-avx")
         (dash-script      (integration-path dash-script-name))
         (comment-status-script
           (integration-path "comment status script.sh.lisp"))
         (shebang-script   (concatenate
                            'string *integration-bin-directory*
                            "shebang argument mode 猫.sh.lisp"))
         (login-program    (concatenate 'string
                                        *integration-bin-directory*
                                        "login-cclsh"))
         (pipe-input       (integration-path "argument-mode-input.sh.lisp"))
         (history-text     "__ARGUMENT_MODE_HISTORY_SHOULD_NOT_LOAD__")
         (script-arguments '("" "-lc" "echo not-executed" "sp ace"
                             "猫 λ" "--no-avx" "--stack-size"
                             "16777216" "-Iignored-image" "--"))
         (script-contents
           "(format t \"__SCRIPT_ONLY__~%\")
(format t \"__SCRIPT_STARTUP__~a__~%\"
        (if (boundp '*argument-startup-loaded*)
            (symbol-value '*argument-startup-loaded*)
            \"absent\"))
(format t \"__SCRIPT_ARGV__~s__~%\" *argv*)
false
  ; unmatched \" and trailing \\
(format t \"__SCRIPT_COMMENT_STATUS__~d__~%\" *last-status*)
(defcommand script-argv-worker (label)
  (format t \"__SCRIPT_~a_ARGV__~s__~%\" label *argv*)
  0)
(pipe (script-argv-worker \"PIPE\"))
(multiple-value-bind (text status)
    (capture (script-argv-worker \"CAPTURE\"))
  (format t \"~a~%__SCRIPT_CAPTURE_STATUS__~d__~%\" text status))
")
         (command
           (format nil
                   "(progn
                      (format t
                              \"__ARG_STARTUP__~~a__ARG_HISTORY__~~s__~~%\"
                              (if (boundp '*argument-startup-loaded*)
                                  (symbol-value '*argument-startup-loaded*)
                                  \"absent\")
                              (find ~s cclsh::*history* :test #'string=))
                      (values))"
                   history-text))
         (configured-environment
           (integration-environment-replace
            (cons "XDG_CONFIG_HOME" xdg)
            (cons "CCLSH_SAFE" "")))
         (safe-environment
           (integration-environment-replace
            (cons "XDG_CONFIG_HOME" xdg)
            (cons "CCLSH_SAFE" "1"))))
    (integration-write-file
     startup
     "(setf *argument-startup-loaded* \"loaded\")\n")
    (integration-write-file history (format nil "~s~%" history-text))
    (integration-write-file script script-contents)
    (integration-write-file
     comment-status-script
     (format nil "false~%  ; final unmatched ~c and trailing ~c~%"
             #\" #\\))
    (integration-write-file
     help-script
     (format nil "(format t \"__HELP_SCRIPT_EXECUTED__~~%\")~%"))
    (integration-write-file dash-script script-contents)
    (integration-write-program
     "shebang argument mode 猫.sh.lisp"
     (format nil "#!~a~%~a" *integration-binary* script-contents))
    (integration-write-file
     pipe-input
     (format nil
             "false~%  ; unmatched ~c and trailing ~c~%
(format t \"__PIPE_COMMENT_STATUS__~~d__~~%\" *last-status*)~%~a~%exit 0~%"
             #\" #\\ command))

    (dolist (flags '(("-lc") ("-cl") ("-ilc") ("-ic")
                     ("-l" "-c") ("-xlc")))
      (let* ((arguments (append flags (list command)))
             (result
               (integration-run-arguments
                arguments :environment configured-environment)))
        (integration-require-success
         result (format nil "configured flags ~s" flags))
        (integration-ensure
         (integration-contains-p
          "__ARG_STARTUP__loaded__ARG_HISTORY__NIL__"
          (direct-result-output result))
         "configured flags ~s did not load only startup.lisp: ~a"
         flags (integration-tail (direct-result-output result)))))

    (let ((result
            (integration-run-arguments
             (list "-c" command) :environment configured-environment)))
      (integration-require-success result "plain -c")
      (integration-ensure
       (integration-contains-p
        "__ARG_STARTUP__absent__ARG_HISTORY__NIL__"
        (direct-result-output result))
       "plain -c loaded user state: ~a"
       (integration-tail (direct-result-output result))))

    (let* ((comment (format nil "  ; unmatched ~c and trailing ~c"
                            #\" #\\))
           (result (integration-run-arguments (list "-c" comment))))
      (integration-require-success result "comment-only -c")
      (integration-ensure
       (and (string= "" (direct-result-output result))
            (string= "" (direct-result-error-output result)))
       "comment-only -c produced output: ~a / ~a"
       (integration-tail (direct-result-output result))
       (integration-tail (direct-result-error-output result))))

    (let ((result
            (integration-run-arguments
             (list "-lc" command) :environment safe-environment)))
      (integration-require-success result "safe -lc")
      (integration-ensure
       (integration-contains-p
        "__ARG_STARTUP__absent__ARG_HISTORY__NIL__"
        (direct-result-output result))
       "CCLSH_SAFE did not suppress configured command state: ~a"
       (integration-tail (direct-result-output result))))

    (let ((result
            (integration-run-arguments
             (list "-ilc") :environment configured-environment)))
      (integration-ensure
       (= 2 (direct-result-status result))
       "missing combined -c operand returned ~d instead of 2"
       (direct-result-status result))
      (integration-ensure
       (integration-contains-p "-c requires an argument"
                               (direct-result-error-output result))
       "missing combined -c operand lacked its diagnostic: ~a"
       (integration-tail (direct-result-error-output result))))

    (dolist (case '(("overview" nil "help")
                    ("single section" ("scripting") "help scripting")
                    ("generated command" ("cd") "help cd")
                    ("multiple sections" ("editing" "unicode")
                     "help editing unicode")))
      (destructuring-bind (name sections command) case
        (let* ((arguments (cons "help" sections))
               (manual
                 (integration-run-arguments
                  arguments
                  :directory *integration-directory*
                  :environment configured-environment))
               (interactive
                 (integration-run-arguments
                  (list "-c" command)
                  :directory *integration-directory*
                  :environment configured-environment)))
          (integration-require-success
           manual (format nil "command-line manual ~a" name))
          (integration-require-success
           interactive (format nil "command-mode manual ~a" name))
          (integration-ensure
           (and (string= (direct-result-output manual)
                         (direct-result-output interactive))
                (string= (direct-result-error-output manual)
                         (direct-result-error-output interactive)))
           "command-line manual ~a differs from interactive help: ~a"
           name (integration-tail (direct-result-output manual)))
          (integration-ensure
           (not (integration-contains-p
                 "__HELP_SCRIPT_EXECUTED__"
                 (direct-result-output manual)))
           "command-line manual ~a executed the local help script"
           name))))

    (let* ((section "definitely-missing-section")
           (manual
             (integration-run-arguments
              (list "help" section)
              :directory *integration-directory*))
           (interactive
             (integration-run-arguments
              (list "-c" (format nil "help ~a" section))
              :directory *integration-directory*)))
      (integration-ensure
       (and (= 1 (direct-result-status manual))
            (= (direct-result-status manual)
               (direct-result-status interactive))
            (string= (direct-result-output manual)
                     (direct-result-output interactive))
            (string= (direct-result-error-output manual)
                     (direct-result-error-output interactive)))
       "unknown command-line manual section differs from interactive help: ~a"
       (integration-tail (direct-result-error-output manual))))

    (let ((result
            (integration-run-arguments
             (list "--" help-script-name "literal argument")
             :directory *integration-directory*)))
      (integration-require-success result "escaped help script")
      (integration-ensure
       (integration-contains-p "__HELP_SCRIPT_EXECUTED__"
                               (direct-result-output result))
       "-- help did not execute the local help script: ~a"
       (integration-tail (direct-result-output result))))

    (let ((result (integration-run-arguments (list "-xyz"))))
      (integration-require-success result "unknown short flags"))

    (let* ((expected (cons script script-arguments))
           (result
             (integration-run-arguments
              (cons script script-arguments)
              :environment configured-environment)))
      (integration-require-script-arguments
       result expected "direct script argument boundary"))

    (let ((result
            (integration-run-arguments (list comment-status-script))))
      (integration-ensure
       (= 1 (direct-result-status result))
       "a final script comment changed status 1 to ~d: ~a"
       (direct-result-status result)
       (integration-tail (direct-result-error-output result))))

    (let* ((expected (cons dash-script-name script-arguments))
           (result
             (integration-run-arguments
              (list* "--" dash-script-name script-arguments)
              :directory *integration-directory*
              :environment configured-environment)))
      (integration-require-script-arguments
       result expected "dash-prefixed script argument boundary"))

    (let* ((expected (cons shebang-script script-arguments))
           (result
             (integration-run-arguments
              script-arguments
              :program shebang-script
              :environment configured-environment)))
      (integration-require-script-arguments
       result expected "shebang script argument boundary"))

    (dolist (program (list *integration-binary* login-program))
      (let* ((context (if (string= program *integration-binary*)
                          "reserved direct command operand"
                          "reserved login command operand"))
             (result
               (integration-run-arguments
                (list "-c" "--no-avx")
                :program program)))
        (integration-require-success result context)
        (integration-ensure
         (integration-contains-p "__RESERVED_COMMAND_OPERAND__"
                                 (direct-result-output result))
         "~a was consumed by the CCL kernel: ~a"
         context
         (integration-tail (direct-result-error-output result)))))

    (let ((result
            (integration-run-arguments
             nil :input pipe-input :environment configured-environment)))
      (integration-require-success result "piped input")
      (integration-ensure
       (and (integration-contains-p
             "__PIPE_COMMENT_STATUS__1__"
             (direct-result-output result))
            (integration-contains-p
             "__ARG_STARTUP__absent__ARG_HISTORY__NIL__"
             (direct-result-output result)))
       "piped input loaded user state: ~a"
       (integration-tail (direct-result-output result))))

    (let ((result
            (integration-run-arguments nil :input comment-status-script)))
      (integration-ensure
       (= 1 (direct-result-status result))
       "a final piped comment changed status 1 to ~d: ~a"
       (direct-result-status result)
       (integration-tail (direct-result-error-output result))))))

(defun integration-check-default-prompt ()
  "Require the built-in prompt to ignore provider executables on PATH."
  (let ((count-file (integration-path "prompt-count")))
    (ignore-errors (delete-file count-file))
    (let* ((result
             (integration-run
              "(let ((cclsh:*prompt-function* nil))
                 (format t \"__DEFAULT_PROMPT__~a__DEFAULT_PROMPT_END__~%\"
                         (cclsh::ansi-strip
                          (cclsh::prompt-render 0 0 80))))"))
           (output (direct-result-output result)))
      (integration-require-success result "provider-neutral default prompt")
      (integration-ensure
       (and (integration-contains-p "__DEFAULT_PROMPT__" output)
            (integration-contains-p "@" output)
            (integration-contains-p "(CCLSH-USER)" output))
       "built-in prompt omitted identity or package: ~a"
       (integration-tail output))
      (integration-ensure
       (not (probe-file count-file))
       "built-in prompt executed a provider discovered on PATH"))))

(defun integration-check-package-environment ()
  "Require package changes to reach prompt renderers and external children."
  (let* ((package-name "CCLSH-INTEGRATION-猫-PACKAGE")
         (prompt-file  (integration-path "prompt-package"))
         (result
           (integration-run
            "(progn
               (cclsh::startup-load)
               (defpackage #:cclsh-integration-猫-package (:use #:cl))
               (in-package #:cclsh-integration-猫-package)
               (cclsh:setenv \"CCLSH_PACKAGE\" \"STALE\")
               (multiple-value-bind (output status)
                   (cclsh::pipeline-capture
                    (list (list \"/usr/bin/printenv\"
                                \"CCLSH_PACKAGE\")))
                 (format t \"__PACKAGE_CHILD__~a:~d__~%\"
                         output status))
               (cclsh:setenv \"CCLSH_PACKAGE\" \"STALE\")
               (cclsh::prompt-render 0 0 80)
               (format t \"__PACKAGE_PROCESS__~a__~%\"
                       (cclsh:getenv \"CCLSH_PACKAGE\"))
               (values))"))
         (output (direct-result-output result)))
    (integration-require-success result "current package environment")
    (integration-ensure
     (integration-contains-p
      (format nil "__PACKAGE_CHILD__~a:0__" package-name) output)
     "external child did not inherit the current package: ~a"
     (integration-tail output))
    (integration-ensure
     (integration-contains-p
      (format nil "__PACKAGE_PROCESS__~a__" package-name) output)
     "prompt rendering did not refresh the process package: ~a"
     (integration-tail output))
    (integration-ensure
     (and (probe-file prompt-file)
          (string= (format nil "~a~%" package-name)
                   (integration-read-file prompt-file)))
     "configured prompt did not inherit the current package")))

(defun integration-check-baked-quicklisp ()
  "Require the saved image to contain a working Quicklisp entry point."
  (let* ((result
           (integration-run
            "(let* ((package (find-package \"QL\"))
                    (quickload (and package
                                    (find-symbol \"QUICKLOAD\" package)))
                    (available (and quickload (fboundp quickload) t))
                    (home (and (find-package \"QL-SETUP\")
                               ql-setup:*quicklisp-home*))
                    (expected
                      (merge-pathnames
                       \"cclsh/quicklisp/\"
                       (uiop:ensure-directory-pathname
                        (pathname (cclsh:getenv \"XDG_DATA_HOME\"))))))
               (when available
                 (funcall quickload :quicklisp :silent t))
               (let* ((quicklisp-system
                        (asdf:find-system \"quicklisp\" nil))
                      (quicklisp-source
                        (and quicklisp-system
                             (asdf:system-source-file quicklisp-system)))
                      (registered (asdf:registered-systems))
                      (portable
                        (and home
                             (uiop:subpathp home expected)
                             (probe-file (merge-pathnames
                                          \"setup.lisp\" home))
                             quicklisp-source
                             (uiop:subpathp quicklisp-source home)
                             (not (find-package \"CCLSH-BUILD\"))
                             (not (member \"cclsh\" registered
                                          :test #'string=))
                             (not (member \"clinedi\" registered
                                          :test #'string=))
                             (not (member \"cl-colorist\" registered
                                          :test #'string=))
                             t)))
                 (format t \"__BAKED_QUICKLISP__~s:~s__~%\"
                         available portable)))"))
         (output (direct-result-output result)))
    (integration-require-success result "baked Quicklisp")
    (integration-ensure
     (integration-contains-p "__BAKED_QUICKLISP__T:T__" output)
     "saved image has no portable QL:QUICKLOAD: ~a"
     (integration-tail output))))

(defun integration-check-lazy-quicklisp-registry ()
  "Require packaged setup to defer and preserve ASDF source discovery."
  (let* ((system-name "cclsh-lazy-registry-probe")
         (asd
           (integration-path
            (format nil "common-lisp/~a/~a.asd"
                    system-name system-name))))
    (integration-write-file
     asd
     (format nil "(asdf:defsystem ~s)~%" system-name))
    (let* ((result
             (integration-run
              (format
               nil
               "(let* ((parameter~%
                          '(:source-registry~%
                            :ignore-inherited-configuration))~%
                        (initially-lazy~%
                          (and~%
                           (not~%
                            (asdf/source-registry:source-registry-initialized-p))~%
                           (null asdf:*source-registry-parameter*))))~%
                  (asdf:initialize-source-registry parameter)~%
                  (let ((setup-result~%
                          (cclsh::quicklisp-packaged-setup))~%
                        (setup-preserved~%
                          (and~%
                           (asdf/source-registry:source-registry-initialized-p)~%
                           (equal parameter~%
                                  asdf:*source-registry-parameter*))))~%
                    (setf asdf:*source-registry-parameter* nil)~%
                    (asdf:clear-source-registry)~%
                    (let ((discovered~%
                            (asdf:find-system ~s nil)))~%
                      (format t~%
                              \"__LAZY_QUICKLISP__~~s:~~s:~~s:~~s__~~%\"~%
                              initially-lazy~%
                              setup-result~%
                              setup-preserved~%
                              (and discovered t)))))"
               system-name)))
           (output (direct-result-output result)))
      (integration-require-success result "lazy Quicklisp registry")
      (integration-ensure
       (integration-contains-p "__LAZY_QUICKLISP__T:T:T:T__" output)
       "packaged setup eagerly rebuilt or disturbed ASDF discovery: ~a"
       (integration-tail output)))))

(defun integration-check-baked-clinedi ()
  "Require the saved image to contain pinned, working editor dependencies."
  (let* ((result
           (integration-run
            "(let* ((package (find-package \"CLINEDI\"))
                    (editor (and package (find-symbol \"EDIT-LINE\" package)))
                    (width-function
                      (and package
                           (find-symbol \"TEXT-CELL-WIDTH\" package)))
                    (width (and width-function (fboundp width-function)
                                (funcall width-function \"猫\")))
                    (commit cclsh:*cclsh-build-clinedi-commit*)
                    (colorist-package (find-package \"CL-COLORIST\"))
                    (strip-ansi
                      (and colorist-package
                           (find-symbol \"STRIP-ANSI\" colorist-package)))
                    (colorist-commit
                      cclsh:*cclsh-build-cl-colorist-commit*)
                    (available (and editor (fboundp editor)
                                    (= width 2)
                                    (stringp commit)
                                    (= (length commit) 40)
                                    strip-ansi
                                    (fboundp strip-ansi)
                                    (string=
                                     \"red\"
                                     (funcall
                                      strip-ansi
                                      (format nil \"~c[31mred~c[0m\"
                                              (code-char 27)
                                              (code-char 27))))
                                    (stringp colorist-commit)
                                    (= (length colorist-commit) 40))))
               (format t \"__BAKED_CLINEDI__~s__~%\" available))"))
         (output (direct-result-output result)))
    (integration-require-success result "baked Clinedi")
    (integration-ensure
     (integration-contains-p "__BAKED_CLINEDI__T__" output)
      "saved image has no pinned working editor dependencies: ~a"
      (integration-tail output))))

(defun integration-check-zoxide ()
  "Require transactional setup, tracking and z/zi queries to share cd hooks."
  (let* ((root                (integration-path "zoxide-root/"))
         (initial-add-fail    (integration-path
                               "zoxide-root/initial-add-fail/"))
         (tracked             (integration-path "zoxide-root/tracked/"))
         (direct              (integration-path "zoxide-root/direct/"))
         (missing             (integration-path "zoxide-root/missing/"))
         (query-target        (integration-path "zoxide-root/query/"))
         (interactive-target  (integration-path
                               "zoxide-root/interactive/"))
         (add-fail            (integration-path "zoxide-root/add-fail/"))
         (zoxide-program      (concatenate
                               'string *integration-bin-directory* "zoxide"))
         (log-path            (integration-path "zoxide.log")))
    (dolist (directory (list initial-add-fail tracked direct query-target
                             interactive-target add-fail))
      (ensure-directories-exist
       (concatenate 'string directory ".keep")))
    (integration-write-file
     (integration-path "zoxide-result")
     (format nil "~a~%" (string-right-trim "/" query-target)))
    (integration-write-file
     (integration-path "zoxide-interactive-result")
     (format nil "~a~%" (string-right-trim "/" interactive-target)))
    (ignore-errors (delete-file log-path))
    (let* ((form
             (format nil
                     "(progn
                        (cd ~s)
                        (format t \"__ZO_SETUP_FAIL__~~d__~~%\"
                                (zoxide-setup))
                        (format t \"__ZO_SETUP_FAIL_UNBOUND__~~s__~~%\"
                                (not
                                 (or (boundp 'z)
                                     (boundp 'zi)
                                     (member
                                      'cclsh::zoxide--directory-change
                                      *directory-change-hooks*))))
                        (cd ~s)
                        (format t \"__ZO_SETUP_1__~~d__~~%\"
                                (zoxide-setup))
                        (format t \"__ZO_SETUP_2__~~d__~~%\"
                                (zoxide-setup))
                        (format t \"__ZO_TRACKED__~~d__~~%\" (cd ~s))
                        (format t \"__ZO_DIRECT__~~d__~~%\" (z ~s))
                        (format t \"__ZO_LITERAL_MISSING__~~d__~~%\"
                                (z \"--\" ~s))
                        (format t \"__ZO_QUERY__~~d__~~%\"
                                (cclsh::dispatch-line \"z keyword\"))
                        (format t \"__ZO_QUERY_PWD__~~a__~~%\" (getenv \"PWD\"))
                        (format t \"__ZO_INTERACTIVE__~~d__~~%\" (zi \"pick\"))
                        (format t \"__ZO_INTERACTIVE_PWD__~~a__~~%\"
                                (getenv \"PWD\"))
                        (format t \"__ZO_EMPTY__~~d__~~%\" (z \"empty\"))
                        (format t \"__ZO_EMPTY_PWD__~~a__~~%\" (getenv \"PWD\"))
                        (format t \"__ZO_FAIL__~~d__~~%\" (z \"fail\"))
                        (format t \"__ZO_FAIL_PWD__~~a__~~%\" (getenv \"PWD\"))
                        (format t \"__ZO_ADD_FAIL__~~d__~~%\" (cd ~s))
                        (delete-file ~s)
                        (setenv \"PATH\" ~s)
                        (format t \"__ZO_MISSING_SETUP__~~d__~~%\"
                                (zoxide-setup))
                        (multiple-value-bind (kind target)
                            (cclsh::command-resolve-fresh \"z\")
                          (declare (ignore target))
                          (format t \"__ZO_EXTERNAL_Z__~~s__~~%\" kind))
                        (multiple-value-bind (kind target)
                            (cclsh::command-resolve-fresh \"zi\")
                          (declare (ignore target))
                          (format t \"__ZO_EXTERNAL_ZI__~~s__~~%\" kind))
                        (run \"/usr/bin/true\"))"
                     initial-add-fail root tracked direct missing
                     add-fail zoxide-program *integration-bin-directory*))
           (result (integration-run form))
           (output (direct-result-output result))
           (log    (and (probe-file log-path)
                        (integration-read-file log-path))))
      (integration-require-success result "zoxide integration")
      (dolist (marker '("__ZO_SETUP_FAIL__9__"
                        "__ZO_SETUP_FAIL_UNBOUND__T__"
                        "__ZO_SETUP_1__0__"
                        "__ZO_SETUP_2__0__"
                        "__ZO_TRACKED__0__"
                        "__ZO_DIRECT__0__"
                        "__ZO_LITERAL_MISSING__1__"
                        "__ZO_QUERY__0__"
                        "__ZO_INTERACTIVE__0__"
                        "__ZO_EMPTY__1__"
                        "__ZO_FAIL__7__"
                        "__ZO_ADD_FAIL__0__"
                        "__ZO_MISSING_SETUP__127__"
                        "__ZO_EXTERNAL_Z__:EXTERNAL__"
                        "__ZO_EXTERNAL_ZI__:EXTERNAL__"))
        (integration-ensure
         (integration-contains-p marker output)
         "zoxide output lacks ~a: ~a" marker (integration-tail output)))
      (integration-ensure
       (integration-contains-p
        (format nil "__ZO_QUERY_PWD__~a__"
                (string-right-trim "/" query-target))
        output)
       "z did not enter query result: ~a" (integration-tail output))
      (integration-ensure
       (integration-contains-p
        (format nil "__ZO_INTERACTIVE_PWD__~a__"
                (string-right-trim "/" interactive-target))
        output)
       "zi did not enter interactive result: ~a" (integration-tail output))
      (integration-ensure
       (integration-contains-p
        (format nil "__ZO_FAIL_PWD__~a__"
                (string-right-trim "/" interactive-target))
       output)
       "failed z query changed directory: ~a" (integration-tail output))
      (integration-ensure
       (integration-contains-p
        (format nil "__ZO_EMPTY_PWD__~a__"
                (string-right-trim "/" interactive-target))
        output)
       "empty z query changed directory: ~a" (integration-tail output))
      (let ((expected
              (format nil
                      "add|--|~a~%add|--|~a~%add|--|~a~%add|--|~a~%~
                       query|--exclude|~a|--|keyword~%add|--|~a~%~
                       query|--interactive|--|pick~%add|--|~a~%~
                       query|--exclude|~a|--|empty~%~
                       query|--exclude|~a|--|fail~%~
                       add|--|~a~%"
                      (string-right-trim "/" initial-add-fail)
                      (string-right-trim "/" root)
                      (string-right-trim "/" tracked)
                      (string-right-trim "/" direct)
                      (string-right-trim "/" direct)
                      (string-right-trim "/" query-target)
                      (string-right-trim "/" interactive-target)
                      (string-right-trim "/" interactive-target)
                      (string-right-trim "/" interactive-target)
                      (string-right-trim "/" add-fail))))
        (integration-ensure
         (and log (string= expected log))
         "zoxide argv/tracking log mismatch:~%expected ~s~%actual ~s"
         expected log)))))

(defun integration-check-no-polling ()
  "Require process and job state changes to be event driven."
  (dolist (path '("source/process.lisp"
                  "source/jobs.lisp"
                  "source/pipeline.lisp"))
    (let ((source (string-downcase (integration-read-file path))))
      (integration-ensure (null (search "/proc/" source))
                          "~a still reads /proc for process state" path)
      (integration-ensure (null (search "(sleep " source))
                          "~a still sleeps while polling process state" path))))

(defun integration-check-process-groups ()
  "Require direct and dynamically launched stages to share one group."
  (labels ((check (form context)
             (let* ((result (integration-run form))
                    (output (direct-result-output result))
                    (clean  (integration-clean-text output))
                    (start  (search "__PGIDS__" clean)))
               (integration-require-success result context)
               (integration-ensure
                start "~a did not print PGIDs: ~a" context clean)
               (let* ((left-start (+ start (length "__PGIDS__")))
                      (colon (position #\: clean :start left-start))
                      (end (and colon
                                (position-if-not
                                 #'digit-char-p clean
                                 :start (1+ colon)))))
                 (integration-ensure
                  colon "malformed ~a PGID output: ~a" context clean)
                 (let ((left
                         (parse-integer clean :start left-start :end colon
                                       :junk-allowed t))
                       (right
                         (parse-integer clean :start (1+ colon)
                                       :end (or end (length clean))
                                       :junk-allowed t)))
                   (integration-ensure
                    (and left right (= left right))
                    "~a stages used PGIDs ~s and ~s"
                    context left right)))
               (integration-ensure
                (zerop (or (integration-integer-after
                            "__PGID_STATUS__" output)
                           -1))
                "~a returned a nonzero status" context))))
    (check
     "(multiple-value-bind (text status)
          (capture (pgid-left) (pgid-right))
        (format t \"__PGID_CAPTURE__~a__PGID_STATUS__~d~%\"
                text status))"
     "direct pipeline")
    (check
     "(progn
        (defcommand integration-run-pgid-left ()
          (run \"pgid-left\"))
        (multiple-value-bind (text status)
            (capture (integration-run-pgid-left) (pgid-right))
          (format t \"__PGID_CAPTURE__~a__PGID_STATUS__~d~%\"
                  text status)))"
     "pipeline-aware run")))

(defun integration-check-fast-leaders ()
  "Stress pipelines whose process-group leader exits immediately."
  (let ((result
          (integration-run
           "(progn
              (loop repeat 100
                    for status = (pipe (true) (true))
                    unless (zerop status)
                      do (error \"fast pipeline returned ~d\" status))
              (format t \"__FAST_LEADERS_OK__~%\"))"
           :timeout 20)))
    (integration-require-success result "fast pipeline leaders")
    (integration-ensure
     (integration-contains-p "__FAST_LEADERS_OK__"
                             (direct-result-output result))
     "fast-leader loop did not finish")))

(defun integration-check-sigpipe ()
  "Stress YES into HEAD and require the deciding status to remain zero."
  (let ((result
          (integration-run
           "(progn
              (defcommand integration-run-yes ()
                (run \"yes\"))
              (loop repeat 40
                    do (multiple-value-bind (text status)
                           (capture (yes) (head \"-n\" \"1\"))
                         (unless (and (string= text \"y\")
                                      (zerop status))
                           (error \"yes/head produced ~s with status ~d\"
                                  text status))))
              (loop repeat 40
                    do (multiple-value-bind (text status)
                           (capture (integration-run-yes)
                                    (head \"-n\" \"1\"))
                         (unless (and (string= text \"y\")
                                      (zerop status))
                           (error
                            \"nested yes/head produced ~s with status ~d\"
                            text status))))
              (format t \"__SIGPIPE_OK__~%\"))"
           :timeout 20)))
    (integration-require-success result "yes/head SIGPIPE stress")
    (integration-ensure
     (integration-contains-p "__SIGPIPE_OK__" (direct-result-output result))
     "yes/head stress did not finish")))

(defun integration-check-ignored-input ()
  "Require a builtin that ignores its input to close the upstream pipe."
  (let* ((result
           (integration-run
            "(progn
               (defcommand ignore-input ()
                 (format t \"ignored-input-ok~%\")
                 0)
               (format t \"__IGNORE_STATUS__~d~%\"
                       (pipe (yes) (ignore-input))))"))
         (status (integration-integer-after
                  "__IGNORE_STATUS__" (direct-result-output result))))
    (integration-require-success result "builtin ignoring input")
    (integration-ensure (eql status 0)
                        "builtin ignoring input returned ~s" status)
    (integration-ensure
     (integration-contains-p "ignored-input-ok" (direct-result-output result))
     "builtin output was lost")))

(defun integration-check-large-stream ()
  "Require a builtin middle stage to relay a pipe larger than buffers."
  (let* ((result
           (integration-run
            "(progn
               (defcommand relay-lines ()
                 (loop for line = (read-line *standard-input* nil nil)
                       while line
                       do (write-line line))
                 0)
               (multiple-value-bind (text status)
                   (capture (seq \"1\" \"50000\")
                            (relay-lines)
                            (wc \"-l\"))
                 (format t \"__LARGE_COUNT__~a__LARGE_STATUS__~d~%\"
                         text status)))"
            :timeout 20))
         (output (direct-result-output result)))
    (integration-require-success result "large builtin stream")
    (integration-ensure
     (integration-contains-p "__LARGE_COUNT__50000" output)
     "large stream was truncated: ~a" (integration-tail output))
    (integration-ensure
     (zerop (or (integration-integer-after "__LARGE_STATUS__" output) -1))
     "large stream returned a nonzero status")))

(defun integration-check-utf8 ()
  "Exercise UTF-8 text boundaries independently of the process locale."
  (let* ((text "Příliš žluťoučký kůň 🐈")
         (printf-format (format nil "%s~%"))
         (file (integration-path "výstup-žluťoučký.txt"))
         (program-name "utf8-žluťoučký")
         (program (concatenate 'string *integration-bin-directory*
                               program-name))
         (script
           (integration-write-file
            (integration-path "skript-žluť-你好.sh.lisp")
            (format nil "(progn (write-line ~s) (values))~%exit 0~%"
                    (concatenate 'string "__UTF8_SCRIPT__" text))))
         (form
           (format nil
                   "(progn
                      (cclsh::startup-load)
                      (format t \"__UTF8_DEFAULT__~~s~~%\"
                              ccl:*default-file-character-encoding*)
                      (defcommand emit-unicode ()
                        (write-line ~s)
                        0)
                      (defcommand relay-unicode ()
                        (loop for line = (read-line *standard-input* nil nil)
                              while line do (write-line line))
                        0)
                      (multiple-value-bind (value status)
                          (capture (emit-unicode) (cat))
                        (format t
                                \"__UTF8_BUILTIN__~~a__UTF8_A_STATUS__~~d~~%\"
                                value status))
                      (multiple-value-bind (value status)
                          (capture (printf ~s ~s)
                                   (relay-unicode))
                        (format t
                                \"__UTF8_EXTERNAL__~~a__UTF8_B_STATUS__~~d~~%\"
                                value status))
                      (format t \"__UTF8_FILE_STATUS__~~d~~%\"
                              (pipe (emit-unicode) (to ~s)))
                      (multiple-value-bind (value status)
                          (capture (from ~s) (cat))
                        (format t
                                \"__UTF8_FILE_READ__~~a__UTF8_D_STATUS__~~d~~%\"
                                value status))
                      (multiple-value-bind (value status)
                          (capture (~s ~s))
                        (format t
                                \"__UTF8_EXEC__~~a__UTF8_C_STATUS__~~d~~%\"
                                value status))
                      (multiple-value-bind (value status)
                          (capture (~s ~s))
                        (format t
                                \"__UTF8_PATH_EXEC__~~a__UTF8_E_STATUS__~~d~~%\"
                                value status))
                      (multiple-value-bind (value status)
                          (capture (~s ~s))
                        (format t
                                \"~~a__UTF8_F_STATUS__~~d~~%\"
                                value status))
                      (let ((ccl:*default-file-character-encoding*
                              :iso-8859-1))
                        (format t
                                \"__UTF8_PROMPT__~~a__UTF8_PROMPT_END__~~%\"
                                (cclsh::prompt-render 0 0 80))))"
                   text printf-format text file file program text
                   program-name text *integration-binary* script))
         (result (integration-run form :timeout 15))
         (output (direct-result-output result)))
    (integration-require-success result "UTF-8 pipelines")
    (integration-ensure
     (integration-contains-p "__UTF8_DEFAULT__:UTF-8" output)
     "saved image did not establish UTF-8 as its file default: ~a"
     (integration-tail output))
    (dolist (marker '("__UTF8_BUILTIN__"
                      "__UTF8_EXTERNAL__"
                      "__UTF8_FILE_READ__"
                      "__UTF8_EXEC__"
                      "__UTF8_PATH_EXEC__"
                      "__UTF8_SCRIPT__"))
      (integration-ensure
       (integration-contains-p (concatenate 'string marker text) output)
       "UTF-8 value after ~a was corrupted: ~a" marker
       (integration-tail output)))
    (integration-ensure
     (integration-contains-p
      "__UTF8_PROMPT__你好 🐈 λ " output)
     "UTF-8 prompt output was corrupted: ~a"
     (integration-tail output))
    (dolist (marker '("__UTF8_A_STATUS__"
                      "__UTF8_B_STATUS__"
                      "__UTF8_C_STATUS__"
                      "__UTF8_D_STATUS__"
                      "__UTF8_E_STATUS__"
                      "__UTF8_F_STATUS__"
                      "__UTF8_FILE_STATUS__"))
      (integration-ensure
       (zerop (or (integration-integer-after marker output) -1))
       "UTF-8 operation ~a failed" marker))
    (integration-ensure
     (string= (format nil "~a~%" text) (integration-read-file file))
     "UTF-8 redirected file did not round-trip")))

(defun integration-check-utf8-config ()
  "Require saved-image history and startup files to decode as UTF-8."
  (let* ((text "Příliš žluťoučký kůň 🐈 你好")
         (xdg (integration-path "konfigurace-žluť-你好/"))
         (config (concatenate 'string
                              (string-right-trim "/" xdg)
                              "/cclsh/"))
         (history (concatenate 'string config "history"))
         (startup (concatenate 'string config "startup.lisp")))
    (integration-write-file history (format nil "~s~%" text))
    (integration-write-file
     startup
     (format nil "(setf cclsh-user::*integration-utf8-startup* ~s)~%"
             text))
    (let* ((result
             (integration-run
              (format nil
               "(progn
                 (setenv \"XDG_CONFIG_HOME\" ~s)
                 (setf (fill-pointer cclsh::*history*) 0)
                 (when (boundp '*integration-utf8-startup*)
                   (makunbound '*integration-utf8-startup*))
                 (let ((ccl:*default-file-character-encoding*
                         :iso-8859-1))
                   (cclsh::history-load)
                   (cclsh::startup-load))
                 (format t \"__UTF8_HISTORY__~~a__UTF8_HISTORY_END__~~%\"
                         (and (plusp (fill-pointer cclsh::*history*))
                              (aref cclsh::*history* 0)))
                 (format t \"__UTF8_STARTUP__~~a__UTF8_STARTUP_END__~~%\"
                         (and (boundp '*integration-utf8-startup*)
                              *integration-utf8-startup*)))"
               xdg)))
           (output (direct-result-output result)))
      (integration-require-success result "UTF-8 configuration files")
      (dolist (marker '("__UTF8_HISTORY__" "__UTF8_STARTUP__"))
        (integration-ensure
         (integration-contains-p (concatenate 'string marker text) output)
         "saved-image configuration value after ~a was corrupted: ~a"
         marker (integration-tail output))))))

(defun integration-check-unicode-environment ()
  "Require Unicode environment mutation, lookup and child inheritance."
  (let* ((name "CCLSH_UTF8_INHERITANCE")
         (unicode-name "CCLSH_UTF8_ŽLUŤ_你好")
         (text "Příliš žluťoučký kůň 🐈 你好")
         (shell-code (format nil "printf '%s' \"$~a\"" name))
         (form
           (format nil
                   "(progn
                      (setenv ~s ~s)
                      (setenv ~s ~s)
                      (format t \"__ENV_GET__~~a__ENV_GET_END__~~%\"
                              (getenv ~s))
                      (multiple-value-bind (value status)
                          (capture (~s \"sh\" \"-c\" ~s))
                        (format t
                                \"__ENV_CHILD__~~a__ENV_CHILD_END__~~d~~%\"
                                value status))
                      (multiple-value-bind (value status)
                          (capture (~s))
                        (format t
                                \"__ENV_UNICODE_NAME__~~a__ENV_UNICODE_NAME_END__~~d~~%\"
                                value status))
                      (unset ~s)
                      (unset ~s)
                      (format t \"__ENV_UNSET__~~s:~~s~~%\"
                              (getenv ~s) (getenv ~s)))"
                   name text unicode-name text name
                   "/usr/bin/env" shell-code "/usr/bin/env"
                   name unicode-name name unicode-name))
         (result (integration-run form))
         (output (direct-result-output result)))
    (integration-require-success result "Unicode environment inheritance")
    (integration-ensure
     (integration-contains-p
      (format nil "__ENV_GET__~a__ENV_GET_END__" text) output)
     "GETENV did not return the exact Unicode value: ~a"
     (integration-tail output))
    (integration-ensure
     (integration-contains-p
      (format nil "__ENV_CHILD__~a__ENV_CHILD_END__0" text) output)
     "child environment did not inherit the exact Unicode value: ~a"
     (integration-tail output))
    (integration-ensure
     (integration-contains-p
      (format nil "~a=~a~%" unicode-name text)
      output)
     "child environment lost a Unicode name or value: ~a"
     (integration-tail output))
    (integration-ensure
     (integration-contains-p "__ENV_UNICODE_NAME_END__0" output)
     "child environment listing failed: ~a"
     (integration-tail output))
    (integration-ensure
     (integration-contains-p "__ENV_UNSET__NIL:NIL" output)
     "UNSET did not remove the Unicode environment entries: ~a"
     (integration-tail output))))

(defun integration-check-binary-copy ()
  "Require redirect-only pipelines to copy every octet exactly."
  (let* ((input (integration-path "binární-vstup-🐈.dat"))
         (output (integration-path "binární-výstup-🐈.dat"))
         (octets (make-array 65539 :element-type '(unsigned-byte 8))))
    (dotimes (index (length octets))
      (setf (aref octets index) (mod (+ (* index 73) 19) 256)))
    (integration-write-octets input octets)
    (let* ((result
             (integration-run
              (format nil
                      "(format t \"__BINARY_COPY__~~d~~%\"
                               (pipe (from ~s) (to ~s)))"
                      input output)))
           (status
             (integration-integer-after
              "__BINARY_COPY__" (direct-result-output result))))
      (integration-require-success result "redirect-only binary copy")
      (integration-ensure (eql status 0)
                          "redirect-only copy returned ~s" status)
      (integration-ensure
       (and (probe-file output)
            (equalp octets (integration-read-octets output)))
       "redirect-only copy changed binary data"))))

(defun integration-check-presentation ()
  "Require captures and redirected builtin streams to contain no ANSI."
  (let* ((output-file (integration-path "plain-output.txt"))
         (error-file  (integration-path "plain-error.txt"))
         (form
           (format nil
                   "(progn
                      (defcommand painted ()
                        (format t \"~~a~~%\"
                                (cclsh::ansi-colorize \"PLAIN-OUT\" :red))
                        (format *error-output* \"~~a~~%\"
                                (cclsh::ansi-colorize \"PLAIN-ERR\" :blue))
                        0)
                      (multiple-value-bind (text status)
                          (capture (painted) (merge-error))
                        (format t
                                \"__PAINT_CAPTURE__~~a__PAINT_STATUS__~~d~~%\"
                                text status))
                      (format t \"__PAINT_FILE_STATUS__~~d~~%\"
                              (pipe (painted)
                                    (error-to ~s)
                                    (to ~s))))"
                   error-file output-file))
         (result (integration-run form))
         (output (direct-result-output result)))
    (integration-require-success result "plain capture and redirection")
    (integration-ensure (and (probe-file output-file) (probe-file error-file))
                        "redirected presentation files were not created")
    (let ((redirected-output (integration-read-file output-file))
          (redirected-error  (integration-read-file error-file)))
      (integration-ensure (null (find (code-char 27) output))
                          "capture contains an ANSI escape")
      (integration-ensure (null (find (code-char 27) redirected-output))
                          "redirected stdout contains an ANSI escape")
      (integration-ensure (null (find (code-char 27) redirected-error))
                          "redirected stderr contains an ANSI escape")
      (integration-ensure
       (and (integration-contains-p "PLAIN-OUT" output)
            (integration-contains-p "PLAIN-ERR" output)
            (string= redirected-output (format nil "PLAIN-OUT~%"))
            (string= redirected-error (format nil "PLAIN-ERR~%")))
       "plain presentation output was lost or changed"))))

(defun integration-check-dev-full ()
  "Require output failures from external and builtin stages to be nonzero."
  (let* ((external
           (integration-run
            "(format t \"__FULL_EXTERNAL__~d~%\"
                     (pipe (yes) (head \"-c\" \"65536\")
                           (to \"/dev/full\")))"))
         (external-status
           (integration-integer-after "__FULL_EXTERNAL__"
                                      (direct-result-output external)))
         (builtin
           (integration-run
            "(progn
               (defcommand fill-full ()
                 (dotimes (index 65536)
                   (declare (ignore index))
                   (write-char #\\x))
                 (finish-output)
                 0)
               (format t \"__FULL_BUILTIN__~d~%\"
                       (pipe (fill-full) (to \"/dev/full\"))))"))
         (builtin-status
           (integration-integer-after "__FULL_BUILTIN__"
                                      (direct-result-output builtin))))
    (integration-ensure
     (or (plusp (direct-result-status external))
         (and external-status (plusp external-status)))
     "external /dev/full failure was hidden: exit ~d, output ~a, error ~a"
     (direct-result-status external)
     (integration-tail (direct-result-output external))
     (integration-tail (direct-result-error-output external)))
    (integration-ensure
     (or (plusp (direct-result-status builtin))
         (and builtin-status (plusp builtin-status)))
     "builtin /dev/full failure was hidden: exit ~d, output ~a, error ~a"
     (direct-result-status builtin)
     (integration-tail (direct-result-output builtin))
     (integration-tail (direct-result-error-output builtin)))))

(defun integration-check-redirect-arity ()
  "Require every redirect pseudo-stage to enforce its exact arity."
  (dolist (case
            `(("to with no path" . "(pipe (echo \"x\") (to))")
              ("to with two paths" .
               ,(format nil "(pipe (echo \"x\") (to ~s ~s))"
                        (integration-path "one") (integration-path "two")))
              ("from with two paths" .
               ,(format nil "(pipe (from ~s ~s) (cat))"
                        (integration-path "one") (integration-path "two")))
              ("merge-error with an argument" .
               "(capture (echo \"x\") (merge-error \"extra\"))")))
    (let ((result (integration-run (rest case))))
      (integration-ensure
       (plusp (direct-result-status result))
       "~a was accepted: ~a" (first case)
       (integration-tail (direct-result-output result))))))

(defun integration-delay (seconds)
  "Wait SECONDS without polling."
  (ccl:timed-wait-on-semaphore (ccl:make-semaphore) seconds)
  (values))

(defun integration-process-alive-p (pid)
  "True when PID still names a process visible to this user."
  (zerop (ccl:external-call "kill" :int pid :int 0 :int)))

(defun integration-check-exit-observer (name)
  "Require exit observer NAME to have received HUP and terminated."
  (let* ((prefix      (integration-path (format nil "exit-~a" name)))
         (pid-path    (concatenate 'string prefix ".pid"))
         (events-path (concatenate 'string prefix ".events")))
    (integration-ensure (probe-file pid-path)
                        "exit observer ~a never recorded its pid" name)
    (let ((pid
            (parse-integer
             (string-trim '(#\space #\tab #\newline #\return)
                          (integration-read-file pid-path)))))
      (unwind-protect
          (progn
            (loop repeat 40
                  until (and (probe-file events-path)
                             (not (integration-process-alive-p pid)))
                  do (integration-delay 0.05))
            (integration-ensure
             (and (probe-file events-path)
                  (integration-contains-p
                   "HUP" (integration-read-file events-path)))
             "exit observer ~a did not receive SIGHUP" name)
            (integration-ensure
             (not (integration-process-alive-p pid))
             "exit observer ~a survived shell shutdown as pid ~d"
             name pid))
        (when (integration-process-alive-p pid)
          (ccl:external-call "kill" :int (- pid) :int 9 :int))))))

(defun integration-check-job-exit-signals ()
  "Require orderly exit to hang up running and confirmed stopped jobs."
  (let* ((running-input
           (integration-write-file
            (integration-path "exit-running.sh.lisp")
            (format nil "exit-observer running &~%sleep 1~%")))
         (running-result
           (integration-run-arguments nil :input running-input :timeout 5)))
    (integration-require-success running-result "running job shell exit")
    (integration-check-exit-observer "running"))
  (let* ((stopped-input
           (integration-write-file
            (integration-path "exit-stopped.sh.lisp")
            (format nil "exit-observer stopped stopped &~%~
                         sleep 1~%exit~%exit 0~%")))
         (stopped-result
           (integration-run-arguments nil :input stopped-input :timeout 5)))
    (integration-require-success stopped-result "stopped job shell exit")
    (let* ((warning "there are stopped jobs")
           (error-output (direct-result-error-output stopped-result))
           (warning-count
             (loop with start = 0
                   for found = (search warning error-output :start2 start)
                   while found
                   count found
                   do (setf start (+ found (length warning))))))
      (integration-ensure
       (= warning-count 1)
       "stopped job confirmation printed ~d warnings instead of one: ~a"
       warning-count (integration-tail error-output)))
    (integration-check-exit-observer "stopped")))

(defun integration-check-failure-cleanup ()
  "Require redirect and spawn failures to leave no children or effects."
  (let* ((marker (integration-path "side-effect"))
         (missing-output (integration-path "missing/directory/out"))
         (redirect-result
           (integration-run
            (format nil "(pipe (mark-and-sleep) (to ~s))"
                    missing-output))))
    (integration-ensure (plusp (direct-result-status redirect-result))
                        "bad redirect unexpectedly succeeded")
    (integration-ensure (null (probe-file marker))
                        "a stage ran before redirect preflight finished"))
  (let* ((pid-path (integration-path "spawn.pid"))
         (spawn-result
           (integration-run "(pipe (record-sleeper) (bad-program))")))
    (integration-ensure (plusp (direct-result-status spawn-result))
                        "bad executable unexpectedly succeeded")
    (when (probe-file pid-path)
      (let ((pid (parse-integer
                  (string-trim '(#\space #\tab #\newline #\return)
                               (integration-read-file pid-path)))))
        (loop repeat 20
              while (integration-process-alive-p pid)
              do (integration-delay 0.05))
        (when (integration-process-alive-p pid)
          (ccl:external-call "kill" :int pid :int 9 :int)
          (integration-fail "spawn failure leaked child pid ~d" pid)))))
  (let ((result
          (integration-run
           (format nil
                   "(progn
                      (dotimes (index 25)
                        (declare (ignore index))
                        (ignore-errors
                          (pipe (from ~s) (cat))))
                      (format t \"__REDIRECT_CLEANUP_OK__~%\"))"
                   (integration-path "does-not-exist")))))
    (integration-require-success result "repeated redirect failures")
    (integration-ensure
     (integration-contains-p "__REDIRECT_CLEANUP_OK__"
                             (direct-result-output result))
     "shell did not survive repeated redirect failures"))
  (let ((result
          (integration-run
           "(progn
              (defparameter *injected-task-ran* nil)
              (defcommand injected-task ()
                (setf *injected-task-ran* t)
                0)
              (dotimes (index 25)
                (declare (ignore index))
                (let ((starts 0)
                      (starter cclsh::*pipeline-task-starter*))
                  (let ((cclsh::*pipeline-task-starter*
                          (lambda (&rest arguments)
                            (incf starts)
                            (if (= starts 2)
                                (error \"injected task start failure\")
                                (apply starter arguments)))))
                    (handler-case
                        (capture (injected-task))
                      (error () nil)))))
              (format t \"__TASK_START_CLEANUP__~s~%\"
                      *injected-task-ran*))")))
    (integration-require-success result "partial task-start cleanup")
    (integration-ensure
     (integration-contains-p "__TASK_START_CLEANUP__NIL"
                             (direct-result-output result))
     "a builtin ran or cleanup stalled after task-start failure: ~a"
     (integration-tail (direct-result-output result)))))


;;;; -- Interactive PTY driver --

(defstruct integration-session
  "One cclsh instance running below util-linux SCRIPT's PTY."
  process
  session-id
  input
  output
  reader-process
  (lock (ccl:make-lock "cclsh integration transcript"))
  (event (ccl:make-semaphore))
  (buffer (make-array 4096
                      :element-type 'character
                      :adjustable t
                      :fill-pointer 0))
  (prompt-number 0 :type integer))

(defun integration-shell-quote (text)
  "Quote TEXT as one POSIX shell word."
  (with-output-to-string (stream)
    (write-char #\' stream)
    (loop for char across text
          do (if (char= char #\')
                 (write-string "'\\''" stream)
                 (write-char char stream)))
    (write-char #\' stream)))

(defun integration-session-command-line ()
  "Return SCRIPT's shell command for the isolated cclsh session."
  (let ((parts
          (append (list "/usr/bin/env" "-i")
                  (mapcar (lambda (pair)
                            (format nil "~a=~a" (first pair) (rest pair)))
                          (integration-environment-replace
                           (cons "CCLSH_SAFE" "")))
                  (list *integration-binary*))))
    (format nil "~{~a~^ ~}" (mapcar #'integration-shell-quote parts))))

(defun integration-session-read-loop (session)
  "Drain SESSION's PTY transcript until SCRIPT closes it."
  (handler-case
      (loop for char = (read-char (integration-session-output session)
                                  nil nil)
            while char
            do (ccl:with-lock-grabbed ((integration-session-lock session))
                 (vector-push-extend char
                                     (integration-session-buffer session)))
               (ccl:signal-semaphore (integration-session-event session)))
    (serious-condition () nil))
  (ccl:signal-semaphore (integration-session-event session))
  (values))

(defun integration-session-text (session)
  "Return an atomic snapshot of SESSION's transcript."
  (ccl:with-lock-grabbed ((integration-session-lock session))
    (coerce (integration-session-buffer session) 'string)))

(defun integration-session-position (session)
  "Return the current transcript length for SESSION."
  (ccl:with-lock-grabbed ((integration-session-lock session))
    (length (integration-session-buffer session))))

(defun integration-session-wait (session marker &key (start 0) (timeout 8))
  "Wait for MARKER in SESSION at or after START and return its position."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      (let* ((text (integration-session-text session))
             (found (search marker text :start2 (min start (length text)))))
        (when found
          (return found))
        (let ((remaining
                (/ (- deadline (get-internal-real-time))
                   internal-time-units-per-second)))
          (when (<= remaining 0)
            (integration-fail
             "timed out waiting for ~s; transcript tail:~%~a"
             marker (integration-tail text)))
          (ccl:timed-wait-on-semaphore
           (integration-session-event session) (min remaining 0.25)))))))

(defun integration-session-send (session text)
  "Send raw TEXT to SESSION's PTY."
  (write-string text (integration-session-input session))
  (force-output (integration-session-input session))
  (values))

(defun integration-session-send-line (session line)
  "Send LINE and a newline to SESSION."
  (integration-session-send session (concatenate 'string line (string #\newline))))

(defun integration-session-await-prompt (session start &key (timeout 8))
  "Wait for SESSION's next uniquely numbered prompt."
  (let* ((number (incf (integration-session-prompt-number session)))
         (marker (format nil "__CCLSH_PROMPT_~d__" number)))
    (integration-session-wait session marker :start start :timeout timeout)))

(defun integration-session-command (session line &key (timeout 8))
  "Run interactive LINE and return its transcript through the next prompt."
  (let ((start (integration-session-position session)))
    (integration-session-send-line session line)
    (let ((end (integration-session-await-prompt session start
                                                  :timeout timeout)))
      (subseq (integration-session-text session) start end))))

(defun integration-session-stop (session)
  "Stop SESSION and close its streams, even after a failed check."
  (when session
    (ignore-errors
      (integration-session-send-line session "stty echo"))
    (ignore-errors
      (integration-session-send-line session "exit"))
    (unless (ccl:timed-wait-on-semaphore
             (ccl::external-process-completed
              (integration-session-process session)) 2)
      (integration-signal-session
       (integration-session-session-id session) "TERM")
      (unless (ccl:timed-wait-on-semaphore
               (ccl::external-process-completed
                (integration-session-process session)) 1)
        (integration-kill-session
         (integration-session-session-id session))
        (ccl:timed-wait-on-semaphore
         (ccl::external-process-completed
          (integration-session-process session)) 2)))
    (ignore-errors (close (integration-session-input session)))
    (ignore-errors (close (integration-session-output session)))
    (ignore-errors
      (ccl:join-process (integration-session-reader-process session))))
  (values))

(defun integration-session-start ()
  "Start an isolated interactive saved-image session below a real PTY."
  (ignore-errors (delete-file (integration-path "prompt-count")))
  (let ((session-path (integration-output-path "pty-sid"))
        (session nil)
        (ready nil))
    (unwind-protect
        (let* ((process
                 (ccl:run-program
                  "/usr/bin/setsid"
                  (list "--wait"
                        (concatenate 'string *integration-bin-directory*
                                     "pty-session")
                        session-path
                        "-qefc" (integration-session-command-line)
                        "/dev/null")
                  :input ':stream
                  :output ':stream
                  :error ':output
                  :wait nil
                  :external-format ':utf-8)))
          (setf session
                (make-integration-session
                 :process process
                 :session-id (ccl:external-process-id process)
                 :input (ccl:external-process-input-stream process)
                 :output (ccl:external-process-output-stream process)))
          (loop repeat 100
                until (probe-file session-path)
                do (sleep 0.01))
          (when (probe-file session-path)
            (setf (integration-session-session-id session)
                  (parse-integer
                   (string-trim '(#\space #\tab #\newline #\return)
                                (integration-read-file session-path)))))
          (setf (integration-session-reader-process session)
                (ccl:process-run-function
                 "cclsh integration PTY reader"
                 (lambda () (integration-session-read-loop session))))
          (integration-session-wait session "__CCLSH_PROMPT_1__" :timeout 12)
          (setf (integration-session-prompt-number session) 1)
          (setf ready t)
          session)
      (unless ready
        (integration-session-stop session))
      (ignore-errors (delete-file session-path)))))

(defun integration-session-interrupt (session character start &key (timeout 8))
  "Send control CHARACTER, then return output through the next prompt."
  (integration-session-send session (string character))
  (let ((end (integration-session-await-prompt session start
                                                :timeout timeout)))
    (subseq (integration-session-text session) start end)))

(defun integration-session-last-status (session)
  "Query and return cclsh's previous command status in SESSION."
  (let* ((output
           (integration-session-command
            session
            "(format t \"__LAST_STATUS__~d~%\" *last-status*)"))
         (status (integration-integer-after "__LAST_STATUS__" output)))
    (integration-ensure status "shell did not report *LAST-STATUS*: ~a"
                        (integration-tail output))
    status))

(defun integration-session-builtin-count (session)
  "Query the integration ticker's shared counter in SESSION."
  (let* ((output
           (integration-session-command
            session
            "(format t \"__BUILTIN_COUNT__~d~%\"
                     *integration-builtin-count*)"))
         (count (integration-integer-after "__BUILTIN_COUNT__" output)))
    (integration-ensure count "shell did not report the builtin count: ~a"
                        (integration-tail output))
    count))

(defun integration-check-enhanced-editor-input ()
  "Require enhanced key encodings and balanced terminal reporting controls."
  (let ((session nil)
        (escape (code-char 27)))
    (unwind-protect
        (progn
          (setf session (integration-session-start))
          (integration-session-command
           session
           "(defun integration-enhanced-enter (&rest arguments)
              (format t \"__ENHANCED_ENTER__~s__~%\" arguments))")
          (let ((disable
                  (format nil "~c[<u~c[>4;0m" escape escape))
                (enable
                  (format nil "~c[>4;0m~c[>1u~c[>4;2m"
                          escape escape escape)))
            (labels ((finish-input (text)
                       (let ((start
                               (integration-session-position session)))
                         (integration-session-send session text)
                         (let ((end
                                 (integration-session-await-prompt
                                  session start :timeout 8)))
                           (subseq (integration-session-text session)
                                   start end))))
                     (require-balanced-controls (output label)
                       (let ((disable-start (search disable output)))
                         (integration-ensure
                          disable-start
                          "~a did not disable enhanced keyboard reporting: ~a"
                          label (integration-tail output))
                         (integration-ensure
                          (null
                           (search disable output
                                   :start2 (+ disable-start
                                              (length disable))))
                          "~a completed more than one editor read: ~a"
                          label (integration-tail output))
                         (integration-ensure
                          (search enable output
                                  :start2 (+ disable-start
                                             (length disable)))
                          "~a did not enable reporting for the next prompt: ~a"
                          label (integration-tail output)))))
              (dolist (entry '(("CSI-u Ctrl-C" . "[99;5u")
                               ("modifyOtherKeys Ctrl-C" . "[27;5;99~")))
                (let* ((label (car entry))
                       (output
                         (finish-input
                          (concatenate 'string
                                       (string escape)
                                       (cdr entry)))))
                  (integration-ensure
                   (integration-contains-p
                    "^C" (integration-clean-text output))
                   "~a did not abort the editor: ~a"
                   label (integration-tail output))
                  (require-balanced-controls output label)
                  (integration-ensure
                   (= 130 (integration-session-last-status session))
                   "~a status was not 130" label)))
              (dolist (entry '(("CSI-u modified Enter" . "[13;2u")
                               ("modifyOtherKeys modified Enter"
                                . "[27;2;13~")))
                (let* ((label (car entry))
                       (output
                         (finish-input
                          (concatenate
                           'string
                           "(integration-enhanced-enter 1"
                           (string escape)
                           (cdr entry)
                           "2)"
                           (string #\newline)))))
                  (integration-ensure
                   (integration-contains-p
                    "__ENHANCED_ENTER__(1 2)__"
                    (integration-clean-text output))
                   "~a did not insert a logical newline: ~a"
                   label (integration-tail output))
                  (require-balanced-controls output label)
                  (integration-ensure
                   (zerop (integration-session-last-status session))
                   "~a command returned nonzero" label))))))
      (integration-session-stop session))))

(defun integration-check-unicode-line-editing ()
  "Require PTY cursor movement and deletion to preserve graphemes."
  (let ((session (integration-session-start)))
    (unwind-protect
        (progn
          (integration-session-command
           session
           "(defcommand unicode-args (&rest arguments)
              (format t \"__UNICODE_EDIT__~s__~%\" arguments)
              0)")
          (flet ((finish-edited-line (line edit)
                   (let ((start (integration-session-position session)))
                     (integration-session-send session line)
                     (funcall edit)
                     (integration-session-send session (string #\newline))
                     (let ((end (integration-session-await-prompt
                                 session start :timeout 8)))
                       (subseq (integration-session-text session)
                               start end)))))
            (let* ((combined (format nil "e~c" (code-char #x301)))
                   (output
                     (finish-edited-line
                      (concatenate 'string "unicode-args A" combined "B")
                      (lambda ()
                        (integration-session-send
                         session (string (code-char 1)))
                        (loop repeat (length "unicode-args A")
                              do (integration-session-send
                                  session
                                  (format nil "~c[C" (code-char 27))))
                        (integration-session-send
                         session (string (code-char 4)))))))
              (integration-ensure
               (integration-contains-p
                "__UNICODE_EDIT__(\"AB\")__"
                (integration-clean-text output))
               "Delete split a combining grapheme: ~a"
               (integration-tail output)))
            (let ((output
                    (finish-edited-line
                     "unicode-args A👨‍👩‍👧‍👦B"
                     (lambda ()
                       (integration-session-send
                        session (format nil "~c[D" (code-char 27)))
                       (integration-session-send
                        session (string (code-char 127)))))))
              (integration-ensure
               (integration-contains-p
                "__UNICODE_EDIT__(\"AB\")__"
                (integration-clean-text output))
               "Backspace split a joined emoji grapheme: ~a"
               (integration-tail output)))))
      (integration-session-stop session))))

(defun integration-check-filtered-history ()
  "Require typed substrings to filter Up history traversal."
  (let ((session nil))
    (unwind-protect
        (progn
          (setf session (integration-session-start))
          (integration-session-command
           session
           "(defcommand integration-filtered-history-probe ()
              (format t \"__FILTERED_HISTORY_~a__~%\" \"EXECUTED\")
              0)")
          (integration-session-command
           session "(integration-filtered-history-probe)")
          (integration-session-command session "(values)")
          (let ((start (integration-session-position session)))
            (integration-session-send session "filtered-history")
            (integration-session-send
             session (format nil "~c[A" (code-char 27)))
            (integration-session-send session (string #\newline))
            (let* ((end
                     (integration-session-await-prompt
                      session start :timeout 8))
                   (output
                     (subseq (integration-session-text session)
                             start end))
                   (clean (integration-clean-text output)))
              (integration-ensure
               (integration-contains-p
                "__FILTERED_HISTORY_EXECUTED__" clean)
               "typed substring plus Up did not recall the older match: ~a"
               (integration-tail output)))))
      (integration-session-stop session))))


;;;; -- Interactive job and terminal checks --

(defun integration-check-interactive ()
  "Exercise terminal signals, jobs, resume events and termios retention."
  (let ((session nil))
    (unwind-protect
        (progn
          (setf session (integration-session-start))

          ;; Ctrl-C must reach every member, including the last stage
          ;; whose signal determines the pipeline status.
          (let ((start (integration-session-position session)))
            (integration-session-send-line
             session "(pipe (longproducer) (cat))")
            (integration-session-wait session "__LONG_STARTED__"
                                      :start start :timeout 5)
            (integration-session-interrupt session (code-char 3) start)
            (integration-ensure (= 130 (integration-session-last-status session))
                                "Ctrl-C pipeline status was not 130"))

          ;; Stop a whole pipeline, continue it in the background, then
          ;; attend it again and interrupt it in the foreground.
          (let ((start (integration-session-position session)))
            (integration-session-send-line
             session "(pipe (longproducer) (cat))")
            (integration-session-wait session "__LONG_STARTED__"
                                      :start start :timeout 5)
            (let ((stopped
                    (integration-session-interrupt session (code-char 26)
                                                   start :timeout 8)))
              (integration-ensure
               (integration-contains-p "Stopped"
                                       (integration-clean-text stopped))
               "Ctrl-Z did not announce a stopped job: ~a"
               (integration-tail stopped))))
          (integration-ensure (= 148 (integration-session-last-status session))
                              "Ctrl-Z pipeline status was not 148")
          (let ((jobs (integration-session-command session "jobs")))
            (integration-ensure
             (integration-contains-p "Stopped" (integration-clean-text jobs))
             "jobs did not report the stopped pipeline: ~a"
             (integration-tail jobs)))
          (integration-session-command session "bg")
          (let ((jobs (integration-session-command session "jobs")))
            (integration-ensure
             (integration-contains-p "Running" (integration-clean-text jobs))
             "bg did not produce a running job: ~a"
             (integration-tail jobs)))
          (let ((start (integration-session-position session)))
            (integration-session-send-line session "fg")
            (integration-session-wait session "__LONG_TICK__"
                                      :start start :timeout 5)
            (integration-session-interrupt session (code-char 3) start)
            (integration-ensure (= 130 (integration-session-last-status session))
                                "foreground-resumed pipeline status was not 130"))

          ;; A normally completed program may deliberately change tty
          ;; modes. A stop restores the pre-attendance shell modes, while
          ;; fg reapplies the stopped job's modes before SIGCONT.
          (let ((initial (integration-session-command session "modecheck")))
            (integration-ensure
             (integration-contains-p "__ECHO_ON__" initial)
             "PTY did not begin with echo enabled"))
          (integration-session-command session "set-noecho")
          (let ((changed (integration-session-command session "modecheck")))
            (integration-ensure
             (integration-contains-p "__ECHO_OFF__" changed)
             "completed stty change was not preserved: ~a"
             (integration-tail changed)))
          (integration-session-command session "stty echo")

          (let ((start (integration-session-position session)))
            (integration-session-send-line session "stop-noecho")
            (integration-session-wait session "__NOECHO_SET__"
                                      :start start :timeout 5)
            (integration-session-await-prompt session start :timeout 8))
          (let ((restored (integration-session-command session "modecheck")))
            (integration-ensure
             (integration-contains-p "__ECHO_ON__" restored)
             "stopped job leaked its terminal mode into the shell: ~a"
             (integration-tail restored)))
          (let ((start (integration-session-position session)))
            (integration-session-send-line session "fg")
            (integration-session-wait session "__RESUMED_NOECHO__"
                                      :start start :timeout 5)
            (integration-session-wait session "__MODE_RESUMED__"
                                      :start start :timeout 5)
            ;; Complete one foreground read before interrupting the next one.
            ;; This makes the marker a handshake with the resumed process,
            ;; rather than output emitted just before it begins to wait.
            (integration-session-send-line session "interrupt-ready")
            (integration-session-wait session "__INTERRUPT_READY__"
                                      :start start :timeout 5)
            (integration-session-interrupt session (code-char 3) start))
          (let ((restored (integration-session-command session "modecheck")))
            (integration-ensure
             (integration-contains-p "__ECHO_ON__" restored)
             "terminal mode after resumed completion was not preserved: ~a"
             (integration-tail restored))))
      (integration-session-stop session))))

(defun integration-check-builtin-job-control ()
  "Exercise stop, bg, fg and interrupt for a long in-process builtin."
  (let ((session nil))
    (unwind-protect
        (progn
          (setf session (integration-session-start))
          (integration-session-command
           session
           "(progn
              (defparameter *integration-builtin-count* 0)
              (defparameter *integration-builtin-foreground-count* 0)
              (defcommand integration-ticker ()
                (let ((foreground nil))
                  (loop
                    (multiple-value-bind (terminal-group error-code)
                        (cclsh::terminal-current-foreground)
                      (declare (ignore error-code))
                      (let ((foreground-now
                              (and terminal-group
                                   (/= terminal-group
                                       (cclsh::terminal-own-process-group)))))
                        (when (and foreground-now (not foreground))
                          (incf *integration-builtin-foreground-count*)
                          (format t \"__BUILTIN_FOREGROUND__~d~%\"
                                  *integration-builtin-foreground-count*))
                        (setf foreground foreground-now)))
                    (incf *integration-builtin-count*)
                    (format t \"__BUILTIN_STREAM__~d~%\"
                            *integration-builtin-count*)
                    (force-output)
                    (sleep 0.1)))))")

          ;; The builtin gate opens only after the external group owns the
          ;; terminal. A /dev/null input avoids involving the lazy terminal
          ;; proxy, while CAT remains a directly tracked external last stage.
          (let ((start (integration-session-position session)))
            (integration-session-send-line
             session
             "(pipe (from \"/dev/null\") (integration-ticker) (cat))")
            (integration-session-wait
             session "__BUILTIN_FOREGROUND__1" :start start :timeout 5)
            (integration-session-wait
             session "__BUILTIN_STREAM__1" :start start :timeout 5)
            (let ((stopped
                    (integration-session-interrupt session (code-char 26)
                                                   start :timeout 5)))
              (integration-ensure
               (integration-contains-p "Stopped"
                                       (integration-clean-text stopped))
               "Ctrl-Z did not stop the builtin pipeline: ~a"
               (integration-tail stopped))))
          (integration-ensure
           (= 148 (integration-session-last-status session))
           "stopped builtin pipeline status was not 148")

          ;; The Lisp worker itself must be suspended, not merely blocked
          ;; behind the stopped external consumer.
          (let ((first (integration-session-builtin-count session)))
            (integration-delay 0.4)
            (let ((second (integration-session-builtin-count session)))
              (integration-ensure
               (= first second)
               "builtin kept running while stopped: ~d became ~d"
               first second)
              (let ((jobs (integration-session-command session "jobs")))
                (integration-ensure
                 (integration-contains-p "Stopped"
                                         (integration-clean-text jobs))
                 "jobs lost the stopped builtin pipeline: ~a"
                 (integration-tail jobs)))

              ;; BG resumes both the external process group and the Lisp
              ;; worker. Its counter and stream must become live again.
              (let ((background-start
                      (integration-session-position session)))
                (integration-session-command session "bg")
                (integration-session-wait
                 session "__BUILTIN_STREAM__"
                 :start background-start :timeout 3))
              (integration-delay 0.4)
              (let ((resumed (integration-session-builtin-count session)))
                (integration-ensure
                 (> resumed second)
                 "bg did not resume the builtin: count stayed at ~d"
                 second))
              (let ((jobs (integration-session-command session "jobs")))
                (integration-ensure
                 (integration-contains-p "Running"
                                         (integration-clean-text jobs))
                 "jobs did not report the resumed builtin pipeline: ~a"
                 (integration-tail jobs)))))

          ;; The worker observes the shell group while under BG, then emits a
          ;; new marker only after FG has completed terminal handoff.
          (let ((start (integration-session-position session)))
            (integration-session-send-line session "fg")
            (integration-session-wait
             session "__BUILTIN_FOREGROUND__2"
             :start start :timeout 5)
            (integration-session-interrupt session (code-char 3) start
                                           :timeout 3))
          (integration-ensure
           (= 130 (integration-session-last-status session))
           "interrupted builtin pipeline status was not 130")

          ;; Returning to a prompt is not enough: the in-process worker
          ;; must be gone rather than leaking in the shared image.
          (let ((first (integration-session-builtin-count session)))
            (integration-delay 0.3)
            (let ((second (integration-session-builtin-count session)))
              (integration-ensure
               (= first second)
               "builtin survived Ctrl-C: ~d became ~d" first second)))
          (let ((jobs (integration-session-command session "jobs")))
            (integration-ensure
             (not (or (integration-contains-p
                       "Stopped" (integration-clean-text jobs))
                      (integration-contains-p
                       "Running" (integration-clean-text jobs))))
             "completed builtin pipeline remained in jobs: ~a"
             (integration-tail jobs))))
      (integration-session-stop session))))

(defun integration-check-run-job-control ()
  "Exercise Ctrl-Z, fg and Ctrl-C through a builtin which calls RUN."
  (let ((session nil))
    (unwind-protect
        (progn
          (setf session (integration-session-start))
          (integration-session-command
           session
           "(defcommand integration-run-longproducer ()
              (run \"longproducer\"))")
          (let ((start (integration-session-position session)))
            (integration-session-send-line
             session
             "(pipe (integration-run-longproducer) (cat))")
            (integration-session-wait session "__LONG_STARTED__"
                                      :start start :timeout 5)
            (let ((stopped
                    (integration-session-interrupt
                     session (code-char 26) start :timeout 8)))
              (integration-ensure
               (integration-contains-p
                "Stopped" (integration-clean-text stopped))
               "Ctrl-Z did not stop pipeline-aware run: ~a"
               (integration-tail stopped))))
          (integration-ensure
           (= 148 (integration-session-last-status session))
           "pipeline-aware run stop status was not 148")
          (let ((jobs (integration-session-command session "jobs")))
            (integration-ensure
             (integration-contains-p "Stopped" (integration-clean-text jobs))
             "jobs lost stopped pipeline-aware run: ~a"
             (integration-tail jobs)))
          (let ((start (integration-session-position session)))
            (integration-session-send-line session "fg")
            (integration-session-wait session "__LONG_TICK__"
                                      :start start :timeout 5)
            (integration-session-interrupt
             session (code-char 3) start :timeout 5))
          (integration-ensure
           (= 130 (integration-session-last-status session))
           "interrupted pipeline-aware run status was not 130")
          (let ((jobs (integration-session-command session "jobs")))
            (integration-ensure
             (not (or (integration-contains-p
                       "Stopped" (integration-clean-text jobs))
                      (integration-contains-p
                       "Running" (integration-clean-text jobs))))
             "completed pipeline-aware run remained in jobs: ~a"
             (integration-tail jobs))))
      (integration-session-stop session))))

(defun integration-check-run-tty-input ()
  "Require nested RUN to inherit a first builtin stage's terminal input."
  (let ((session nil)
        (input "dynamic run input: Příliš žluťoučký 🐈"))
    (unwind-protect
        (progn
          (setf session (integration-session-start))
          (integration-session-command
           session
           "(defcommand integration-run-reader ()
              (run \"run-one-line\"))")
          (let ((start (integration-session-position session)))
            (integration-session-send-line
             session
             "(pipe (integration-run-reader) (run-line-counter))")
            (integration-session-wait
             session "__RUN_READER_READY__" :start start :timeout 5)
            (integration-session-send-line session input)
            (let* ((end (integration-session-await-prompt
                         session start :timeout 5))
                   (output (subseq (integration-session-text session)
                                   start end))
                   (clean (integration-clean-text output)))
              (integration-ensure
               (integration-contains-p "__RUN_READER_COUNT__1" clean)
               "nested run input did not cross the pipeline: ~a"
               (integration-tail output))
              (integration-ensure
               (not (integration-contains-p "__RUN_READER_VALUE__" clean))
               "nested run output bypassed the pipeline: ~a"
               (integration-tail output))))
          (integration-ensure
           (zerop (integration-session-last-status session))
           "pipeline-aware run input returned nonzero"))
      (integration-session-stop session))))

(defun integration-check-builtin-tty-input ()
  "Require a first-stage builtin to read the inherited terminal safely."
  (let ((session nil)
        (input "proxy input: Příliš žluťoučký 🐈"))
    (unwind-protect
        (progn
          (setf session (integration-session-start))
          (integration-session-command
           session
           "(defcommand integration-tty-reader ()
              (format t \"__TTY_READER_READY__~d~%\" 1)
              (force-output)
              (handler-case
                  (let ((line (read-line *standard-input* nil nil)))
                    (cond (line
                           (format t \"__TTY_INPUT__~a~%\" line)
                           0)
                          (t
                           (format *error-output* \"__TTY_EOF__~%\")
                           1)))
                (error (condition)
                  (format *error-output* \"__TTY_ERROR__~a~%\" condition)
                  1)))")
          (let ((start (integration-session-position session)))
            (integration-session-send-line
             session "(pipe (integration-tty-reader) (cat))")
            (let ((runtime-start
                    (integration-session-wait
                     session "__TTY_READER_READY__1"
                     :start start :timeout 5)))
              (integration-session-send-line session input)
              (let* ((end (integration-session-await-prompt
                           session start :timeout 5))
                     (output (subseq (integration-session-text session)
                                     runtime-start end))
                     (clean (integration-clean-text output)))
                (integration-ensure
                 (integration-contains-p
                  (concatenate 'string "__TTY_INPUT__" input) clean)
                 "typed terminal input did not reach the builtin: ~a"
                 (integration-tail output))
                (integration-ensure
                 (not (or (integration-contains-p "__TTY_ERROR__" clean)
                          (integration-contains-p "__TTY_EOF__" clean)
                          (integration-contains-p "Input/output error" clean)
                          (integration-contains-p "EIO" clean)))
                 "builtin terminal proxy failed: ~a"
                 (integration-tail output)))))
          (integration-ensure
           (zerop (integration-session-last-status session))
           "builtin terminal-input pipeline returned nonzero"))
      (integration-session-stop session))))


;;;; -- Run --

(unwind-protect
    (progn
      (integration-install-programs)
      (integration-test "Quickloaded pipeline returns to host CCL"
                        #'integration-check-library-pty)
      (integration-test "saved-image startup stress"
                        #'integration-check-image-startup)
      (integration-test "command-line modes and user state"
                        #'integration-check-command-line-modes)
      (integration-test "explicit Lisp glob arguments"
                        #'integration-check-lisp-glob)
      (integration-test "provider-neutral default prompt"
                        #'integration-check-default-prompt)
      (integration-test "current package environment"
                        #'integration-check-package-environment)
      (integration-test "Quicklisp baked into saved image"
                        #'integration-check-baked-quicklisp)
      (integration-test "lazy packaged Quicklisp source registry"
                        #'integration-check-lazy-quicklisp-registry)
      (integration-test "editor dependencies baked into saved image"
                        #'integration-check-baked-clinedi)
      (integration-test "zoxide directory integration"
                        #'integration-check-zoxide)
      (integration-test "event-driven child state"
                        #'integration-check-no-polling)
      (integration-test "one process group per pipeline"
                        #'integration-check-process-groups)
      (integration-test "fast process-group leaders"
                        #'integration-check-fast-leaders)
      (integration-test "SIGPIPE stress"
                        #'integration-check-sigpipe)
      (integration-test "builtin ignores input"
                        #'integration-check-ignored-input)
      (integration-test "large builtin streaming"
                        #'integration-check-large-stream)
      (integration-test "UTF-8 boundaries"
                        #'integration-check-utf8)
      (integration-test "UTF-8 history and startup files"
                        #'integration-check-utf8-config)
      (integration-test "Unicode environment inheritance"
                        #'integration-check-unicode-environment)
      (integration-test "redirect-only binary copy"
                        #'integration-check-binary-copy)
      (integration-test "plain capture and redirection"
                        #'integration-check-presentation)
      (integration-test "/dev/full failures"
                        #'integration-check-dev-full)
      (integration-test "redirect exact arity"
                        #'integration-check-redirect-arity)
      (integration-test "orderly exit job signals"
                        #'integration-check-job-exit-signals)
      (integration-test "spawn and redirect cleanup"
                        #'integration-check-failure-cleanup)
      (integration-test "PTY jobs, signals and terminal modes"
                        #'integration-check-interactive)
      (integration-test "PTY Unicode grapheme editing"
                        #'integration-check-unicode-line-editing)
      (integration-test "PTY enhanced key input and cleanup"
                        #'integration-check-enhanced-editor-input)
      (integration-test "PTY typed substring history search"
                        #'integration-check-filtered-history)
      (integration-test "PTY in-process builtin job control"
                        #'integration-check-builtin-job-control)
      (integration-test "PTY pipeline-aware run job control"
                        #'integration-check-run-job-control)
      (integration-test "PTY builtin terminal-input proxy"
                        #'integration-check-builtin-tty-input)
      (integration-test "PTY pipeline-aware run terminal input"
                        #'integration-check-run-tty-input))
  (ignore-errors
    (uiop:delete-directory-tree *integration-directory*
                                :validate t
                                :if-does-not-exist ':ignore)))

(cond (*integration-failures*
       (format *error-output* "~d integration check~:p failed:~%"
               (length *integration-failures*))
       (dolist (failure (nreverse *integration-failures*))
         (format *error-output* "  ~a~%" failure))
       (ccl:quit 1))
      (t
       (format t "All cclsh integration checks passed.~%")
       (ccl:quit 0)))
