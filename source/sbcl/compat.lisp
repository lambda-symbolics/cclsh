;;;; -- SBCL host compatibility --
;;;
;;; The portable CCLSH layers historically use a compact subset of CCL's
;;; process and synchronization interface.  Keep that vocabulary behind this
;;; package so the shell itself remains implementation-neutral.

(defpackage #:ccl
  (:use #:cl)
  (:export #:run-program
           #:external-process-status
           #:external-process-id
           #:external-process-input-stream
           #:external-process-output-stream
           #:current-directory
           #:quit
           #:process-run-function
           #:make-external-format
           #:external-call
           #:*break-hook*
           #:*command-line-argument-list*
           #:*unprocessed-command-line-arguments*
           #:make-lock
           #:with-lock-grabbed
           #:make-semaphore
           #:wait-on-semaphore
           #:timed-wait-on-semaphore
           #:signal-semaphore
           #:join-process
           #:process-kill
           #:process-suspend
           #:process-resume
           #:process-allow-schedule
           #:without-interrupts
           #:encode-string-to-octets
           #:native-translated-namestring
           #:function-name
           #:find-definition-sources
           #:source-note-p
           #:ensure-source-note-text
           #:source-note-text
           #:source-note-filename
           #:source-note-start-pos
           #:record-source-file))

(in-package #:ccl)

(require :sb-posix)

(defvar *break-hook* nil)
(defvar *command-line-argument-list* nil)
(defvar *unprocessed-command-line-arguments* nil)

(defun current-directory ()
  "Return the current working directory as a pathname."
  (uiop:getcwd))

(defun (setf current-directory) (directory)
  "Change the current working directory to DIRECTORY."
  (uiop:chdir directory)
  directory)

(defun quit (&optional (status 0))
  "Leave the current SBCL process with STATUS."
  (uiop:quit status))

(defun make-lock (name &key read-only)
  "Create a mutual-exclusion lock named NAME.
READ-ONLY is accepted for CCL source compatibility."
  (declare (ignore read-only))
  (bordeaux-threads:make-lock name))

(defmacro with-lock-grabbed ((place) &body body)
  "Evaluate BODY while holding the lock denoted by PLACE."
  `(bordeaux-threads:with-lock-held (,place)
     ,@body))

(defun make-semaphore (&optional (count 0))
  "Create a counting semaphore with initial COUNT."
  (bordeaux-threads:make-semaphore :count count))

(defun wait-on-semaphore (semaphore)
  "Wait until SEMAPHORE can be decremented."
  (bordeaux-threads:wait-on-semaphore semaphore))

(defun timed-wait-on-semaphore (semaphore timeout)
  "Wait up to TIMEOUT seconds for SEMAPHORE."
  (bordeaux-threads:wait-on-semaphore semaphore :timeout timeout))

(defun signal-semaphore (semaphore &optional (count 1))
  "Increment SEMAPHORE by COUNT."
  (bordeaux-threads:signal-semaphore semaphore :count count))

(defun process-run-function (name function &rest arguments)
  "Start FUNCTION with ARGUMENTS in a named Lisp thread."
  (bordeaux-threads:make-thread
   (lambda () (apply function arguments))
   :name name))

(defun join-process (process)
  "Join a Lisp thread started by PROCESS-RUN-FUNCTION."
  (bordeaux-threads:join-thread process))

(defun process-kill (process)
  "Terminate PROCESS when it is a live Lisp thread."
  (when (bordeaux-threads:thread-alive-p process)
    (bordeaux-threads:destroy-thread process))
  nil)

(defun process-suspend (process)
  "SBCL has no safe asynchronous thread suspension operation."
  (declare (ignore process))
  nil)

(defun process-resume (process)
  "SBCL has no counterpart to CCL's process resume operation."
  (declare (ignore process))
  nil)

(defun process-allow-schedule ()
  "Yield the current Lisp thread when another thread can run."
  (bordeaux-threads:thread-yield))

(defmacro without-interrupts (&body body)
  "Evaluate BODY while SBCL defers asynchronous interrupts."
  `(sb-sys:without-interrupts
     ,@body))

(defun encode-string-to-octets (string &key external-format)
  "Encode STRING as UTF-8 octets.
EXTERNAL-FORMAT is accepted for CCL source compatibility."
  (declare (ignore external-format))
  (sb-ext:string-to-octets string :external-format :utf-8))

(defun native-translated-namestring (pathname)
  "Return PATHNAME in the host operating system's native syntax."
  (native-namestring pathname))

(defun getpid ()
  "Return the Unix process identifier of the current SBCL process."
  (sb-posix:getpid))

(defstruct (sbcl-source-note
            (:constructor sbcl--make-source-note
                (&key filename start-pos end-pos source)))
  "Minimal source-note representation for CCLSH's interactive editor."
  filename
  start-pos
  end-pos
  source)

(defun make-source-note (&key filename start-pos end-pos source)
  "Construct a source note compatible with CCLSH's editor protocol."
  (sbcl--make-source-note :filename filename :start-pos start-pos
                          :end-pos end-pos :source source))

(defun function-name (function)
  "Return FUNCTION's recorded name when SBCL exposes one."
  (nth-value 2 (function-lambda-expression function)))

(defun find-definition-sources (designator type)
  "Return no source notes when SBCL lacks CCL's source database.
CCLSH's editor reports a clear unavailable-source error in this case."
  (declare (ignore designator type))
  nil)

(defun source-note-p (note)
  "True when NOTE is CCLSH's SBCL source-note representation."
  (sbcl-source-note-p note))

(defun ensure-source-note-text (note)
  "CCLSH source notes already retain their source octets."
  note)

(defun source-note-text (note)
  "Decode source NOTE text as UTF-8."
  (let ((source (sbcl-source-note-source note)))
    (and source (sb-ext:octets-to-string source :external-format :utf-8))))

(defun source-note-filename (note)
  "Return NOTE's source pathname."
  (sbcl-source-note-filename note))

(defun source-note-start-pos (note)
  "Return NOTE's source offset."
  (sbcl-source-note-start-pos note))

(defun record-source-file (name type note)
  "Accept source registration from EDIT without a global SBCL source database."
  (declare (ignore name type note))
  nil)
