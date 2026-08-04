;;;; -- Prewarmed worker lease --
;;;
;;; A one-shot worker enters this protocol before terminal signal setup or
;;; command-line processing. The native broker supplies the caller's process
;;; state, commits the lease, and then leaves the worker to run the ordinary
;;; interactive shell path.

(in-package #:cclsh)

(defconstant +prewarm-worker-fd-variable+ "CCLSH_FAST_WORKER_FD"
  "Environment variable naming the prewarmed worker control descriptor.")

(defconstant +prewarm-worker-argument+ "--cclsh-internal-prewarm-worker"
  "Private argument that confirms an intentional prewarmed worker launch.")

(defconstant +prewarm-request-magic+ #x43434631
  "Magic number at the start of a prewarmed worker request.")

(defconstant +prewarm-request-version+ 4
  "Supported prewarmed worker protocol version.")

(defconstant +prewarm-request-lease+ 1
  "Request type that leases a prewarmed worker to one interactive shell.")

(defconstant +prewarm-request-header-size+ 144
  "Size in octets of a prewarmed worker request header.")

(defconstant +prewarm-request-body-limit+ 131072
  "Largest accepted prewarmed worker request body in octets.")

(define-condition prewarm-protocol-error (error)
  ((reason
    :initarg :reason
    :reader prewarm-protocol-error-reason))
  (:documentation "Signaled when a prewarmed worker request is malformed.")
  (:report
   (lambda (condition stream)
     (format stream "invalid prewarmed worker request: ~a"
             (prewarm-protocol-error-reason condition)))))

(defun prewarm--protocol-error (control &rest arguments)
  "Signal a protocol error described by CONTROL and ARGUMENTS."
  (error 'prewarm-protocol-error
         :reason (apply #'format nil control arguments)))

(defun prewarm--hard-exit ()
  "Leave a rejected prewarmed worker without running shell cleanup."
  (external-call "_exit" :int 70))

(defun prewarm--descriptor (text)
  "Return the fixed private control descriptor named by TEXT."
  (unless (string= text "3")
    (prewarm--protocol-error "unexpected worker descriptor"))
  3)

(defun prewarm--control-stream (descriptor)
  "Wrap DESCRIPTOR as an unencoded binary input/output stream."
  (ccl::make-fd-stream descriptor
                       :direction ':io
                       :interactive t
                       :element-type '(unsigned-byte 8)
                       :character-p nil
                       :sharing ':lock
                       :auto-close nil))

(defun prewarm--read-exact (stream count)
  "Read exactly COUNT octets from STREAM or signal a protocol error."
  (let ((octets (make-array count :element-type '(unsigned-byte 8))))
    (loop with position = 0
          while (< position count)
          for next = (read-sequence octets stream
                                    :start position
                                    :end count)
          do (when (= next position)
               (prewarm--protocol-error "unexpected control EOF"))
             (setf position next))
    octets))

(defun prewarm--write-byte (stream octet)
  "Write and flush one protocol OCTET to STREAM."
  (write-byte octet stream)
  (finish-output stream)
  (values))

(defun prewarm--u16 (octets offset)
  "Decode one big-endian unsigned 16-bit integer from OCTETS at OFFSET."
  (logior (ash (aref octets offset) 8)
          (aref octets (+ offset 1))))

(defun prewarm--u32 (octets offset)
  "Decode one big-endian unsigned 32-bit integer from OCTETS at OFFSET."
  (loop with value = 0
        for index from offset below (+ offset 4)
        do (setf value (logior (ash value 8) (aref octets index)))
        finally (return value)))

(defun prewarm--decode-utf-8 (octets start end description)
  "Strictly decode OCTETS from START to END as one NUL-free UTF-8 string."
  (when (position 0 octets :start start :end end)
    (prewarm--protocol-error "~a contains NUL" description))
  (handler-bind
      ((ccl:decoding-problem
         (lambda (condition)
           (declare (ignore condition))
           (prewarm--protocol-error "~a is not valid UTF-8" description))))
    (multiple-value-bind (string consumed)
        (ccl:decode-string-from-octets octets
                                      :start start
                                      :end end
                                      :external-format ':utf-8)
      (unless (= consumed end)
        (prewarm--protocol-error "~a is truncated UTF-8" description))
      string)))

(defun prewarm--decode-absolute-path (octets start end description)
  "Decode and validate one nonempty absolute path field."
  (when (= start end)
    (prewarm--protocol-error "~a is empty" description))
  (let ((path (prewarm--decode-utf-8 octets start end description)))
    (unless (and (plusp (length path))
                 (char= (char path 0) #\/))
      (prewarm--protocol-error "~a is not absolute" description))
    path))

(defun prewarm--decode-environment (octets start end)
  "Decode exact NUL-terminated NAME=VALUE entries from OCTETS."
  (when (= start end)
    (return-from prewarm--decode-environment nil))
  (unless (zerop (aref octets (1- end)))
    (prewarm--protocol-error "environment is not NUL-terminated"))
  (loop with bindings = nil
        with position = start
        while (< position end)
        for terminator = (position 0 octets :start position :end end)
        do (unless terminator
             (prewarm--protocol-error "environment entry is unterminated"))
           (when (= position terminator)
             (prewarm--protocol-error "environment contains an empty entry"))
           (let ((equals (position (char-code #\=)
                                   octets
                                   :start position
                                   :end terminator)))
             (unless (and equals (> equals position))
               (prewarm--protocol-error
                "environment entry has no nonempty name"))
             (let* ((entry
                      (prewarm--decode-utf-8 octets position terminator
                                             "environment entry"))
                    (separator (position #\= entry)))
               (push (cons (subseq entry 0 separator)
                           (subseq entry (1+ separator)))
                     bindings)))
           (setf position (1+ terminator))
        finally (return (nreverse bindings))))

(defun prewarm--request (stream)
  "Read and validate one lease request from STREAM.
Return the requested directory, shell, environment and umask."
  (let* ((header      (prewarm--read-exact
                       stream +prewarm-request-header-size+))
         (magic       (prewarm--u32 header 0))
         (version     (prewarm--u16 header 4))
         (type        (prewarm--u16 header 6))
         (body-length (prewarm--u32 header 8))
         (cwd-length  (prewarm--u32 header 12))
         (shell-length (prewarm--u32 header 16))
         (env-length  (prewarm--u32 header 20))
         (umask       (prewarm--u32 header 24))
         (flags       (prewarm--u32 header 28)))
    (unless (= magic +prewarm-request-magic+)
      (prewarm--protocol-error "bad magic"))
    (unless (= version +prewarm-request-version+)
      (prewarm--protocol-error "unsupported version"))
    (unless (= type +prewarm-request-lease+)
      (prewarm--protocol-error "unsupported request type"))
    (unless (zerop flags)
      (prewarm--protocol-error "unsupported request flags"))
    (when (> umask #o777)
      (prewarm--protocol-error "umask is out of range"))
    (dolist (field-length
             (list body-length cwd-length shell-length env-length))
      (when (> field-length +prewarm-request-body-limit+)
        (prewarm--protocol-error "request field is too large")))
    (unless (= body-length (+ cwd-length shell-length env-length))
      (prewarm--protocol-error "body length does not match its fields"))
    (let* ((body        (prewarm--read-exact stream body-length))
           (cwd-end     cwd-length)
           (shell-end   (+ cwd-end shell-length))
           (environment-end (+ shell-end env-length))
           (cwd         (prewarm--decode-absolute-path
                         body 0 cwd-end "working directory"))
           (shell       (prewarm--decode-absolute-path
                         body cwd-end shell-end "shell path"))
           (environment (prewarm--decode-environment
                         body shell-end environment-end)))
      (values cwd shell environment umask))))

(defun prewarm--apply-request (cwd shell environment umask)
  "Install validated process state for a leased worker."
  (environment--replace-exact environment)
  (setf (current-directory) cwd
        *default-pathname-defaults* (current-directory))
  (external-call "umask" :unsigned-int umask :unsigned-int)
  (setenv "SHELL" shell)
  (values))

(defun prewarm-worker-lease (descriptor-text)
  "Lease this one-shot worker over the descriptor named by DESCRIPTOR-TEXT.
Return after the broker commits the lease and the control channel is closed."
  (let ((descriptor (prewarm--descriptor descriptor-text)))
    (with-open-stream (stream (prewarm--control-stream descriptor))
      (prewarm--write-byte stream (char-code #\R))
      (multiple-value-bind (cwd shell environment umask)
          (prewarm--request stream)
        (prewarm--apply-request cwd shell environment umask))
      (prewarm--write-byte stream (char-code #\P))
      (unless (eql (read-byte stream nil nil) (char-code #\G))
        (prewarm--protocol-error "lease was not committed"))))
  (values))
