;;;; -- Builtin commands --
;;;
;;; Commands implemented in Lisp. Each DEFCOMMAND is callable from the
;;; command line by name and from Lisp forms as an ordinary function.

(in-package #:cclsh)

(defcommand exit (&optional status)
  "Exit the shell. STATUS defaults to the last command's status. With
   stopped jobs the first exit only warns; exit again to leave anyway."
  (if (jobs-exit-blocked-p)
      1
      (let ((code (cond ((null status)
                         *last-status*)
                        ((integerp status)
                         status)
                        (t
                         (or (parse-integer (princ-to-string status)
                                            :junk-allowed t)
                             0)))))
        (terminal-restore)
        (shell-quit code))))

(defcommand rehash ()
  "Forget cached PATH lookups and completion candidates."
  (clrhash *path-cache*)
  (setf *path-cache-source* nil)
  (setf *path-command-names* nil)
  (setf *path-command-names-source* nil)
  0)

(defcommand commands ()
  "List the shell commands defined in Lisp."
  (let ((found nil))
    (do-symbols (symbol *package*)
      (when (and (boundp symbol)
                 (typep (symbol-value symbol) 'command))
        (pushnew symbol found)))
    (let* ((sorted (sort found #'string< :key #'symbol-name))
           (width  (loop for symbol in sorted
                         maximize (length (symbol-name symbol)))))
      (dolist (symbol sorted)
        (let* ((instance      (symbol-value symbol))
               (documentation (command-documentation instance))
               (name          (format nil "~va" width
                                      (string-downcase (symbol-name symbol)))))
          (format t "~a  ~a~%"
                  (terminal-colorize name ':cyan)
                  (if documentation
                      (subseq documentation 0
                              (position #\newline documentation))
                      ""))))))
  0)


;;; Environment

(defcommand export (&rest assignments)
  "Set environment variables: export NAME=value... A bare NAME prints
   its current value; no arguments print the whole environment."
  (if (null assignments)
      (dolist (entry (environment-variables))
        (format t "~a~%" entry))
      (dolist (assignment assignments)
        (let ((split (position #\= assignment)))
          (if split
              (setenv (subseq assignment 0 split)
                      (subseq assignment (1+ split)))
              (let ((value (getenv assignment)))
                (if value
                    (format t "~a=~a~%" assignment value)
                    (format *error-output* "export: ~a is unset~%"
                            assignment)))))))
  0)

(defcommand unset (&rest names)
  "Remove environment variables. Names are symbols or strings."
  (dolist (name names)
    (unsetenv name))
  0)


;;; Quicklisp

(defvar *packaged-quicklisp-template* nil
  "Read-only Quicklisp template included by a binary package, or NIL.")

(defvar *packaged-quicklisp-files* nil
  "Sanitized Quicklisp template files embedded in a saved image, or NIL.")

(defvar *packaged-quicklisp-home* nil
  "Writable Quicklisp home selected for this packaged process, or NIL.")

(defvar *packaged-quicklisp-user-setup-home* nil
  "Quicklisp home whose local-init files ran in this process, or NIL.")

(defvar *packaged-quicklisp-last-error* nil
  "Last packaged Quicklisp initialization error already reported.")

(defun quicklisp--directory-pathname (path)
  "Return PATH as an absolute or relative directory pathname."
  (uiop:ensure-directory-pathname (pathname path)))

(defun quicklisp--personal-home ()
  "Return an existing personal Quicklisp home, or NIL."
  (let ((home (merge-pathnames
               "quicklisp/"
               (quicklisp--directory-pathname (home-directory)))))
    (when (probe-file (merge-pathnames "setup.lisp" home))
      home)))

(defun quicklisp--packaged-home ()
  "Return the writable Quicklisp home used by a packaged image."
  (let* ((xdg  (getenv "XDG_DATA_HOME"))
         (base (if (and xdg (plusp (length xdg)))
                   (quicklisp--directory-pathname xdg)
                   (merge-pathnames ".local/share/"
                                    (quicklisp--directory-pathname
                                     (home-directory))))))
    (merge-pathnames "cclsh/quicklisp/" base)))

(defun quicklisp--copy-directory-tree (source target)
  "Copy the regular files and directories below SOURCE into TARGET."
  (ensure-directories-exist target)
  (dolist (file (uiop:directory-files source))
    (uiop:copy-file file (merge-pathnames (file-namestring file) target)))
  (dolist (directory (uiop:subdirectories source))
    (quicklisp--copy-directory-tree
     directory
     (merge-pathnames (enough-namestring directory source) target)))
  target)

(defun quicklisp--copy-embedded-files (files target)
  "Copy embedded Quicklisp FILES below TARGET."
  (dolist (entry files)
    (let* ((relative (first entry))
           (contents (second entry))
           (target-file (merge-pathnames relative target)))
      (unless (and (stringp relative)
                   (uiop:relative-pathname-p (pathname relative))
                   (uiop:subpathp target-file target))
        (error "Unsafe embedded Quicklisp path: ~s" relative))
      (ensure-directories-exist target-file)
      (with-open-file (stream target-file
                              :direction ':output
                              :element-type '(unsigned-byte 8)
                              :if-exists ':error
                              :if-does-not-exist ':create)
        (write-sequence contents stream))))
  target)

(defun quicklisp--copy-packaged-template (target)
  "Copy the packaged Quicklisp template below TARGET."
  (prog1
      (cond (*packaged-quicklisp-template*
             (quicklisp--copy-directory-tree
              (quicklisp--directory-pathname *packaged-quicklisp-template*)
              target))
            (*packaged-quicklisp-files*
             (quicklisp--copy-embedded-files
              *packaged-quicklisp-files* target))
            (t
             (error "Saved image contains no packaged Quicklisp template")))
    (ensure-directories-exist
     (merge-pathnames "local-projects/system-index.txt" target))))

(defun quicklisp--install-packaged-home (target)
  "Atomically initialize TARGET from the packaged Quicklisp template."
  (when (probe-file (merge-pathnames "setup.lisp" target))
    (return-from quicklisp--install-packaged-home target))
  (when (probe-file target)
    (error "Quicklisp home exists without setup.lisp: ~a" target))
  (let* ((parent (uiop:pathname-parent-directory-pathname target))
         (temporary
           (merge-pathnames
            (format nil ".quicklisp.~d.tmp/" (ccl::getpid))
            parent)))
    (ensure-directories-exist parent)
    (when (probe-file temporary)
      (uiop:delete-directory-tree temporary
                                  :validate t
                                  :if-does-not-exist ':ignore))
    (unwind-protect
        (progn
          (quicklisp--copy-packaged-template temporary)
          (handler-case
              (rename-file temporary target)
            (file-error (condition)
              (unless (probe-file (merge-pathnames "setup.lisp" target))
                (error condition))))
          target)
      (when (probe-file temporary)
        (uiop:delete-directory-tree temporary
                                    :validate t
                                    :if-does-not-exist ':ignore)))))

(defun quicklisp--rebase (home)
  "Point the loaded Quicklisp client at writable HOME."
  (let ((quicklisp-home
          (find-symbol "*QUICKLISP-HOME*" '#:ql-setup))
        (local-directories
          (find-symbol "*LOCAL-PROJECT-DIRECTORIES*"
                       '#:quicklisp-client)))
    (unless (and quicklisp-home local-directories)
      (return-from quicklisp--rebase nil))
    (let ((directories (list (merge-pathnames "local-projects/" home))))
      (when *packaged-quicklisp-template*
        (pushnew
         (uiop:pathname-parent-directory-pathname
          (quicklisp--directory-pathname *packaged-quicklisp-template*))
         directories
         :test #'equal))
      (setf (symbol-value quicklisp-home) home
            (symbol-value local-directories) (nreverse directories)))
    (setf asdf:*central-registry*
          (list (merge-pathnames "quicklisp/" home)))
    (setf asdf:*user-cache*
          (uiop:xdg-cache-home "common-lisp" ':implementation))
    (asdf:initialize-output-translations nil)
    ;; The default ASDF registry recursively searches ~/common-lisp. Leave
    ;; that work until the first system lookup instead of delaying every
    ;; shell startup.
    (setf asdf:*source-registry-parameter* nil)
    (asdf:clear-source-registry)
    t))

(defun quicklisp--select-packaged-home ()
  "Select or initialize the writable packaged Quicklisp home."
  (let ((override (getenv "CCLSH_QUICKLISP_HOME")))
    (cond ((and override (plusp (length override)))
           (quicklisp--install-packaged-home
            (quicklisp--directory-pathname override)))
          ((quicklisp--personal-home))
          (t
           (quicklisp--install-packaged-home
            (quicklisp--packaged-home))))))

(defun quicklisp--packaged-p ()
  "Return true when this image contains a packaged Quicklisp template."
  (or *packaged-quicklisp-template* *packaged-quicklisp-files*))

(defun quicklisp--report-packaged-error (condition)
  "Report packaged Quicklisp CONDITION once and return NIL."
  (let ((message (princ-to-string condition)))
    (unless (string= message (or *packaged-quicklisp-last-error* ""))
      (format *error-output*
              "cclsh: packaged Quicklisp unavailable: ~a~%"
              message))
    (setf *packaged-quicklisp-last-error* message))
  nil)

(defun quicklisp-packaged-setup ()
  "Prepare writable Quicklisp state for a binary package."
  (when (and (quicklisp--packaged-p)
             (find-package '#:quicklisp))
    (when *packaged-quicklisp-home*
      (return-from quicklisp-packaged-setup t))
    (handler-case
        (let ((home (quicklisp--select-packaged-home)))
          (unless (quicklisp--rebase home)
            (error "loaded Quicklisp cannot be rebased"))
          (setf *packaged-quicklisp-home* home
                *packaged-quicklisp-last-error* nil)
          t)
      (error (condition)
        (quicklisp--report-packaged-error condition)))))

(defun quicklisp-packaged-user-setup ()
  "Load personal Quicklisp local-init files for a configured session."
  (when (quicklisp-packaged-setup)
    (when (equal *packaged-quicklisp-home*
                 *packaged-quicklisp-user-setup-home*)
      (return-from quicklisp-packaged-user-setup t))
    (handler-case
        (let ((setup (find-symbol "SETUP" '#:quicklisp-client)))
          (unless (and setup (fboundp setup))
            (error "loaded Quicklisp has no setup function"))
          (funcall setup)
          (setf *packaged-quicklisp-user-setup-home*
                *packaged-quicklisp-home*)
          t)
      (error (condition)
        (format *error-output*
                "cclsh: Quicklisp local setup failed: ~a~%"
                condition)
        nil))))

(defun quicklisp-setup ()
  "Make Quicklisp available in the running shell. Loads an existing
   ~/quicklisp/setup.lisp, or downloads and installs Quicklisp with
  curl on first use. Returns true when Quicklisp is available."
  (let ((setup
          (merge-pathnames
           "quicklisp/setup.lisp"
           (quicklisp--directory-pathname (home-directory)))))
    (cond ((find-package '#:quicklisp)
           (if (quicklisp--packaged-p)
               (quicklisp-packaged-user-setup)
               t))
          ((probe-file setup)
           (load setup :verbose nil :external-format ':utf-8)
           t)
          (t
           (let ((bootstrap (concatenate 'string (config-directory)
                                         "quicklisp-bootstrap.lisp")))
             (ensure-directories-exist (config-directory))
             (when (zerop (run "curl" "-fsSL" "-o" bootstrap
                               "https://beta.quicklisp.org/quicklisp.lisp"))
               (load bootstrap :verbose nil :external-format ':utf-8)
               (funcall (find-symbol "INSTALL" '#:quicklisp-quickstart))
               t))))))
