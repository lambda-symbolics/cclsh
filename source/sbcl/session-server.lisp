;;;; -- Shared-image shell server --

(in-package #:cclsh)

(cffi:defcfun ("sh_listen" server--listen) :int (path :string))
(cffi:defcfun ("sh_accept_peer" server--accept) :int (listener :int))
(cffi:defcfun ("sh_shutdown" server--shutdown) :int (descriptor :int))
(cffi:defcfun ("sh_send" server--native-send) :int
  (socket :int) (type :uint32) (token :uint32) (body :pointer)
  (length :uint32) (descriptors :pointer) (count :uint32))
(cffi:defcfun ("sh_receive" server--native-receive) :int
  (socket :int) (type :pointer) (token :pointer) (body :pointer)
  (length :pointer) (descriptors :pointer) (count :pointer))

(defstruct server-session
  "One shell's native client connection and independent shell state."
  socket descriptors directory environment group thread reader
  (threads nil)
  (closed-p nil) (finished-p nil)
  (write-lock (ccl:make-lock "session output"))
  (request-lock (ccl:make-lock "session request"))
  (state-lock (ccl:make-lock "session children"))
  (requests (make-hash-table))
  (token 0)
  (children (make-hash-table))
  (early-events (make-hash-table)))

(defstruct server-request
  "One outstanding native operation, including ownership of reply descriptors."
  result descriptors (event (ccl:make-semaphore)))

(defvar *server-listener* nil)
(defvar *server-stopping* nil)
(defvar *server-sessions* (make-hash-table))
(defvar *server-lock* (ccl:make-lock "shell server"))

(defparameter *server-context-symbols*
  '(*shell-session* *session-environment* *session-process-function*
    *shell-input-fd* *shell-output-fd* *shell-error-fd* *environment-lock*
    ccl::*session-directory* ccl::*session-exit-function*
    ccl::*session-environment-function* ccl::*session-thread-function*
    *default-pathname-defaults* *package* *readtable* *read-eval*
    *standard-input* *standard-output* *error-output* *terminal-io*
    *query-io* *debug-io* *trace-output* *print-pretty* *print-length* *print-level*
    * ** *** / // /// + ++ +++ ccl:*break-hook*
    *jobs* *jobs-touch-counter* *jobs-exit-signaled* *jobs-exit-warned*
    *jobs-exit-confirmed* *history* *history-limit* *last-status*
    *prompt-function* *line-editor-keymap* *line-editor-word-delimiter-mode-p*
    *line-editor-word-delimiters* *presentation-enabled*
    *semantic-prompt-markers-enabled* *semantic-command-marker-active*
    *terminal-saved-termios* *terminal-shell-attributes*
    *terminal-control-signals-active* *directory-change-hooks*
    *directory-change-hooks-running-p* *zoxide-program* *zoxide-z-command*
    *zoxide-zi-command* *path-cache* *path-cache-source*
    *path-command-names* *path-command-names-source* *argv*)
  "Bindings explicitly inherited by shell pipeline threads.")

(defun server--thread-context (function)
  "Capture shell dynamic bindings before starting a pipeline thread."
  (let ((symbols *server-context-symbols*)
        (values (mapcar #'symbol-value *server-context-symbols*))
        (session *shell-session*))
    (lambda ()
      (let ((thread sb-thread:*current-thread*) (run-p nil))
        (ccl:with-lock-grabbed ((server-session-state-lock session))
          (unless (server-session-finished-p session)
            (push thread (server-session-threads session))
            (setf run-p t)))
        (when run-p
          (unwind-protect
              (progv symbols values (funcall function))
            (ccl:with-lock-grabbed ((server-session-state-lock session))
              (setf (server-session-threads session)
                    (delete thread (server-session-threads session))))))))))

(defun server--stop-session-threads (session)
  "Unwind shell-owned pipeline threads when their terminal session ends."
  (let ((threads (ccl:with-lock-grabbed ((server-session-state-lock session))
                   (copy-list (server-session-threads session)))))
    (dolist (thread threads)
      (when (sb-thread:thread-alive-p thread)
        (ignore-errors (sb-thread:terminate-thread thread))))
    (dolist (thread threads)
      (ignore-errors (sb-thread:join-thread thread :timeout 1 :default nil))))
  (values))

(defun server--packet (&key words strings)
  "Encode native integers and bounded UTF-8 strings for the local protocol."
  (let ((result (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer t)))
    (dolist (word words)
      (cffi:with-foreign-object (value :uint32)
        (setf (cffi:mem-ref value :uint32) (ldb (byte 32 0) word))
        (dotimes (index 4)
          (vector-push-extend (cffi:mem-aref value :uint8 index) result))))
    (dolist (string strings)
      (when (find #\Null string) (error "NUL in session protocol string"))
      (map nil (lambda (byte) (vector-push-extend byte result))
           (sb-ext:string-to-octets string :external-format :utf-8))
      (vector-push-extend 0 result))
    (when (> (length result) 131072) (error "Session packet is too large"))
    (coerce result '(simple-array (unsigned-byte 8) (*)))))

(defun server--word (body offset &optional signed)
  "Decode one checked integer from BODY at OFFSET."
  (unless (<= (+ offset 4) (length body)) (error "Truncated session integer"))
  (cffi:with-pointer-to-vector-data (pointer body)
    (let ((value (cffi:mem-ref pointer :uint32 offset)))
      (if (and signed (logbitp 31 value)) (- value (ash 1 32)) value))))

(defun server--strings (body offset)
  "Decode the NUL-terminated string tail of BODY without invoking the reader."
  (loop while (< offset (length body))
        for end = (or (position 0 body :start offset)
                      (error "Unterminated session string"))
        collect (sb-ext:octets-to-string body :start offset :end end
                                             :external-format :utf-8)
        do (setf offset (1+ end))))

(defun server--send (socket type &key (token 0) body descriptors)
  "Send one complete local packet, raising an error on disconnection."
  (let ((body (or body (server--packet))))
    (cffi:with-foreign-object (fds :int (max 1 (length descriptors)))
      (loop for fd in descriptors for index from 0
            do (setf (cffi:mem-aref fds :int index) fd))
      (cffi:with-pointer-to-vector-data (pointer body)
        (let ((code (server--native-send socket type token pointer
                                         (length body) fds (length descriptors))))
          (when (minusp code) (error "Session send failed: errno ~d" (- code)))))))
  (values))

(defun server--receive (socket)
  "Receive TYPE, TOKEN, BODY and owned DESCRIPTORS from SOCKET."
  (cffi:with-foreign-objects ((type :uint32) (token :uint32) (length :uint32)
                            (count :uint32) (fds :int 8) (body :uint8 131072))
    (let ((code (server--native-receive socket type token body length fds count)))
      (when (minusp code) (error "Session receive failed: errno ~d" (- code))))
    (let ((bytes (make-array (cffi:mem-ref length :uint32)
                             :element-type '(unsigned-byte 8))))
      (dotimes (index (length bytes))
        (setf (aref bytes index) (cffi:mem-aref body :uint8 index)))
      (values (cffi:mem-ref type :uint32) (cffi:mem-ref token :uint32) bytes
              (loop for index below (cffi:mem-ref count :uint32)
                    collect (cffi:mem-aref fds :int index))))))

(defun server--session-send (session type &key token body descriptors)
  "Serialize packets sent by threads belonging to SESSION."
  (ccl:with-lock-grabbed ((server-session-write-lock session))
    (server--send (server-session-socket session) type :token (or token 0)
                  :body body :descriptors descriptors)))

(defun server--request (session type &key body descriptors)
  "Perform a native RPC; canceled callers cannot consume another caller's reply."
  (let ((request (make-server-request)) (token nil))
    (ccl:with-lock-grabbed ((server-session-request-lock session))
      (when (server-session-closed-p session) (error "Session disconnected"))
      (setf token (incf (server-session-token session))
            (gethash token (server-session-requests session)) request))
    (unwind-protect
        (progn
          (server--session-send session type :token token :body body
                                              :descriptors descriptors)
          (ccl:wait-on-semaphore (server-request-event request))
          (when (server-session-closed-p session) (error "Session disconnected"))
          (values (server-request-result request)
                  (shiftf (server-request-descriptors request) nil)))
      (ccl:with-lock-grabbed ((server-session-request-lock session))
        (remhash token (server-session-requests session))
        (mapc #'fd-close (server-request-descriptors request))))))

(defun server--child-event (session body)
  "Publish a native child event, retaining events that beat spawn registration."
  (let ((pid (server--word body 0))
        (state (case (server--word body 4)
                 (0 :running) (1 :exited) (2 :signaled) (3 :stopped)
                 (otherwise (error "Invalid session child state"))))
        (code (server--word body 8)))
    (ccl:with-lock-grabbed ((server-session-state-lock session))
      (let ((process (gethash pid (server-session-children session))))
        (if process
            (progn
              (process--publish-state process state code)
              (when (member state '(:exited :signaled))
                (remhash pid (server-session-children session))))
            (push (list state code)
                  (gethash pid (server-session-early-events session)))))))
  (values))

(defun server--disconnect (session)
  "Wake blocked requests and jobs, then unwind the disconnected shell thread."
  (ccl:with-lock-grabbed ((server-session-request-lock session))
    (setf (server-session-closed-p session) t)
    (maphash (lambda (token request)
               (declare (ignore token))
               (ccl:signal-semaphore (server-request-event request)))
             (server-session-requests session)))
  (ccl:with-lock-grabbed ((server-session-state-lock session))
    (maphash (lambda (pid process)
               (declare (ignore pid))
               (process--publish-state process :exited 70))
             (server-session-children session)))
  (unless (server-session-finished-p session)
    (let ((thread (server-session-thread session)))
      (when (and thread (sb-thread:thread-alive-p thread))
        (ignore-errors
          (sb-thread:interrupt-thread
           thread (lambda () (throw 'server-session-exit 70)))))))
  (values))

(defun server--read-events (session)
  "Receive native replies and child changes until the connection closes."
  (unwind-protect
      (handler-case
          (loop
            (multiple-value-bind (type token body descriptors)
                (server--receive (server-session-socket session))
              (case type
                (6
                 (ccl:with-lock-grabbed ((server-session-request-lock session))
                   (let ((request (gethash token (server-session-requests session))))
                     (if request
                         (progn
                           (setf (server-request-result request) (server--word body 0 t)
                                 (server-request-descriptors request) descriptors)
                           (ccl:signal-semaphore (server-request-event request)))
                         (mapc #'fd-close descriptors)))))
                (7
                 (mapc #'fd-close descriptors)
                 (server--child-event session body))
                (otherwise
                 (mapc #'fd-close descriptors)
                 (error "Unexpected session event")))))
        (serious-condition () nil))
    (server--disconnect session)))

(defun server--spawn (session program arguments group fd0 fd1 fd2 environment event)
  "Ask the terminal owner to spawn a child with this session's cwd and env."
  (let* ((pid (server--request
               session 5
               :body (server--packet
                      :words (list group (length arguments) (length environment))
                      :strings (append (list (namestring (current-directory))
                                             (namestring program))
                                       (mapcar #'string arguments) environment))
               :descriptors (list fd0 fd1 fd2))))
    (when (minusp pid)
      (error 'process-spawn-error :program program :operation "spawn"
                                 :code (- pid)))
    (let ((process (process--make pid :event event)))
      (setf (shell-process-session process) session)
      (ccl:with-lock-grabbed ((server-session-state-lock session))
        (setf (gethash pid (server-session-children session)) process)
        (dolist (transition
                 (nreverse (gethash pid (server-session-early-events session))))
          (apply #'process--publish-state process transition))
        (remhash pid (server-session-early-events session))
        (when (eq (shell-process-live-state process) :done)
          (remhash pid (server-session-children session))))
      process)))

(defun server--process-operation (session operation &rest arguments)
  "Implement the shell's process and terminal adapters through the native peer."
  (case operation
    (:spawn (apply #'server--spawn session arguments))
    (:own-group (server-session-group session))
    (:monitor
     (server--request session 12 :body (server--packet :words arguments)))
    (:open
     (destructuring-bind (path flags mode) arguments
       (multiple-value-bind (code descriptors)
           (server--request session 11
                            :body (server--packet :words (list flags mode)
                                                  :strings (list path)))
         (unless (and (zerop code) (= (length descriptors) 1))
           (mapc #'fd-close descriptors)
           (error 'process-spawn-error :program path :operation "open"
                                      :code (abs code)))
         (first descriptors))))
    ((:foreground :kill)
     (let ((code (server--request session (if (eq operation :foreground) 8 9)
                                  :body (server--packet :words arguments))))
       (values (zerop code) (if (minusp code) (- code) 0))))
    (otherwise (error "Unknown session operation ~s" operation))))

(defun server--run-shell (session)
  "Run one existing shell loop with an independent dynamic shell context."
  (let* ((*shell-session* session)
         (*shell-input-fd* (first (server-session-descriptors session)))
         (*shell-output-fd* (second (server-session-descriptors session)))
         (*shell-error-fd* (third (server-session-descriptors session)))
         (*standard-input* (fd-input-stream *shell-input-fd* :auto-close nil))
         (*standard-output* (fd-output-stream *shell-output-fd* :auto-close nil))
         (*error-output* (fd-output-stream *shell-error-fd* :auto-close nil))
         (*terminal-io* (make-two-way-stream *standard-input* *standard-output*))
         (*query-io* *terminal-io*) (*debug-io* *terminal-io*)
         (*trace-output* *error-output*)
         (*package* (find-package :cclsh-user)) (*readtable* (copy-readtable nil))
         (*read-eval* t) (*print-pretty* nil) (*print-length* nil) (*print-level* nil)
         (* nil) (** nil) (*** nil) (/ nil) (// nil) (/// nil)
         (+ nil) (++ nil) (+++ nil) (ccl:*break-hook* nil)
         (*session-environment* (server-session-environment session))
         (*environment-lock* (ccl:make-lock "session environment"))
         (ccl::*session-directory* (server-session-directory session))
         (*default-pathname-defaults* ccl::*session-directory*)
         (ccl::*session-exit-function*
           (lambda (status) (throw 'server-session-exit status)))
         (ccl::*session-environment-function* #'environment-variables)
         (ccl::*session-thread-function* #'server--thread-context)
         (*session-process-function*
           (lambda (operation &rest arguments)
             (apply #'server--process-operation session operation arguments)))
         (*jobs* nil) (*jobs-touch-counter* 0) (*jobs-exit-signaled* nil)
         (*jobs-exit-warned* nil) (*jobs-exit-confirmed* nil)
         (*history* (make-array 0 :adjustable t :fill-pointer t))
         (*history-limit* *history-limit*) (*last-status* 0) (*argv* nil)
         (*prompt-function* *prompt-function*)
         (*line-editor-keymap* (clinedi:copy-keymap *line-editor-keymap*))
         (*line-editor-word-delimiter-mode-p* *line-editor-word-delimiter-mode-p*)
         (*line-editor-word-delimiters* (copy-list *line-editor-word-delimiters*))
         (*presentation-enabled* *presentation-enabled*)
         (*semantic-prompt-markers-enabled* *semantic-prompt-markers-enabled*)
         (*semantic-command-marker-active* nil)
         (*terminal-saved-termios* nil) (*terminal-shell-attributes* nil)
         (*terminal-control-signals-active* t)
         (*directory-change-hooks* (copy-list *directory-change-hooks*))
         (*directory-change-hooks-running-p* nil)
         (*zoxide-program* nil) (*zoxide-z-command* nil) (*zoxide-zi-command* nil)
         (*path-cache* (make-hash-table :test #'equal)) (*path-cache-source* nil)
         (*path-command-names* nil) (*path-command-names-source* nil))
    (unwind-protect
        (catch 'server-session-exit
          (setf (server-session-reader session)
                (sb-thread:make-thread
                 (lambda () (server--read-events session))
                 :name "cclsh native job events"))
          (handler-case (progn (main) 0)
            (serious-condition (condition)
              (dispatch-report-error condition)
              70)))
      (setf (server-session-finished-p session) t)
      (ignore-errors (terminal-restore))
      (ignore-errors (jobs--signal-exit))
      (server--stop-session-threads session)
      (ignore-errors (finish-output *standard-output*))
      (ignore-errors (finish-output *error-output*)))))

(defun server--connection (socket)
  "Negotiate a terminal session or handle a server management request."
  (let ((session nil) (owned-descriptors nil))
    (unwind-protect
        (handler-case
            (multiple-value-bind (type token body descriptors) (server--receive socket)
              (declare (ignore token))
              (setf owned-descriptors descriptors)
              (case type
                ((3 4)
                 (let ((count (ccl:with-lock-grabbed (*server-lock*)
                                (hash-table-count *server-sessions*))))
                   (server--send socket 6
                                 :body (sb-ext:string-to-octets
                                        (format nil "cclshd pid=~d sessions=~d~%"
                                                (sb-posix:getpid) count)
                                        :external-format :utf-8)))
                 (when (= type 4)
                   (setf *server-stopping* t)
                   (server--shutdown *server-listener*)))
                (1
                 (unless (and (= (length descriptors) 3)
                              (every (lambda (fd) (= 1 (terminal--isatty fd))) descriptors))
                   (error "Session requires three terminal descriptors"))
                 (let* ((strings (server--strings body 4))
                        (directory (truename (first strings)))
                        (environment (make-hash-table :test #'equal)))
                   (unless (uiop:directory-pathname-p directory)
                     (error "Session cwd is not a directory"))
                   (dolist (entry (rest strings))
                     (let ((separator (position #\= entry)))
                       (unless (and separator (plusp separator))
                         (error "Malformed session environment"))
                       (setf (gethash (subseq entry 0 separator) environment)
                             (subseq entry (1+ separator)))))
                   (server--send socket 2)
                   (multiple-value-bind (commit token body extra) (server--receive socket)
                     (declare (ignore token body))
                     (mapc #'fd-close extra)
                     (unless (= commit 2) (error "Session was not committed")))
                   (setf session
                         (make-server-session
                          :socket socket :descriptors descriptors :directory directory
                          :environment environment :group (server--word body 0)
                          :thread sb-thread:*current-thread*))
                   (ccl:with-lock-grabbed (*server-lock*)
                     (setf (gethash socket *server-sessions*) session))
                   (let ((status (server--run-shell session)))
                     (server--session-send session 10
                                           :body (server--packet :words (list status))))))
                (otherwise (error "Unsupported session request"))))
          (serious-condition () nil))
      (when session
        (setf (server-session-finished-p session) t)
        (ccl:with-lock-grabbed (*server-lock*)
          (remhash socket *server-sessions*)))
      (server--shutdown socket)
      (when (and session (server-session-reader session))
        (ignore-errors (sb-thread:join-thread (server-session-reader session))))
      (mapc #'fd-close owned-descriptors)
      (fd-close socket))))

(defun server-run (path)
  "Serve independent shell sessions in this SBCL image until STOP is requested."
  (cffi:load-foreign-library
   (asdf:system-relative-pathname "cclsh" "libcclsh-session.so"))
  (let ((listener (server--listen path)))
    (when (minusp listener)
      (error "Cannot listen on ~a: errno ~d" path (- listener)))
    (setf *server-listener* listener *server-stopping* nil)
    ;; Native clients preserve their own umasks for commands and redirects.
    ;; Arbitrary Lisp OPEN calls share this conservative process-wide mask.
    (sb-posix:umask #o077)
    (terminal-signals-setup)
    (unwind-protect
        (loop until *server-stopping*
              for socket = (server--accept listener)
              do (when (minusp socket)
                   (unless *server-stopping* (error "Accept failed: ~d" socket))
                   (return))
                 (sb-thread:make-thread (let ((fd socket))
                                          (lambda () (server--connection fd)))
                                        :name "cclsh session"))
      (let ((sessions (ccl:with-lock-grabbed (*server-lock*)
                        (loop for session being the hash-values of *server-sessions*
                              collect session))))
        (dolist (session sessions)
          (server--shutdown (server-session-socket session)))
        (dolist (session sessions)
          (ignore-errors (sb-thread:join-thread (server-session-thread session)
                                               :timeout 2 :default nil))))
      (fd-close listener)
      (ignore-errors (delete-file path))))
  (values))
