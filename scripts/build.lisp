;;;; -- Build script --
;;;
;;; Loads Quicklisp and the cclsh system into a fresh CCL, then saves a
;;; standalone executable. Run through scripts/build.

;; These defaults must be established before loading ASDF, Quicklisp,
;; or project source. In particular, CCL's ordinary GETENV decodes raw
;; environment bytes as Latin-1 on Linux, so the Quicklisp override is
;; read through libc and decoded explicitly below.
(setf ccl:*default-file-character-encoding* ':utf-8
      ccl:*default-external-format*
      '(:character-encoding :utf-8 :line-termination :unix)
      ccl:*terminal-character-encoding-name* ':utf-8)

(defpackage #:cclsh-build
  (:use #:cl))

(in-package #:cclsh-build)

(defun build-getenv-utf-8 (name)
  "The UTF-8 value of environment variable NAME, or NIL."
  (ccl::with-utf-8-cstr (encoded-name name)
    (let ((value (ccl:external-call "getenv"
                                    :address encoded-name
                                    :address)))
      (unless (ccl:%null-ptr-p value)
        (ccl::%get-utf-8-cstring value)))))

(require :asdf)

(defparameter *build-quicklisp-root-files*
  '("setup.lisp" "asdf.lisp" "client-info.sexp")
  "Root Quicklisp files allowed into the sanitized build template.")

(defparameter *build-quicklisp-client-files*
  '("bundle-template.lisp"
    "bundle.lisp"
    "cdb.lisp"
    "client-info.lisp"
    "client-update.lisp"
    "client.lisp"
    "config.lisp"
    "deflate.lisp"
    "dist-update.lisp"
    "dist.lisp"
    "fetch-gzipped.lisp"
    "http.lisp"
    "impl-util.lisp"
    "impl.lisp"
    "local-projects.lisp"
    "minitar.lisp"
    "misc.lisp"
    "network.lisp"
    "package.lisp"
    "progress.lisp"
    "quicklisp.asd"
    "setup.lisp"
    "utils.lisp"
    "version.txt")
  "Quicklisp client files allowed into the sanitized build template.")

(defparameter *build-quicklisp-dist-files*
  '("distinfo.txt" "enabled.txt" "releases.txt" "systems.txt")
  "Quicklisp distribution files allowed into the sanitized build template.")

(defvar *build-quicklisp-home* nil
  "Temporary sanitized Quicklisp home used by this build.")

(defun build-quicklisp-source-setup-pathname ()
  "The source Quicklisp setup file used to create a sanitized template."
  (let ((override (build-getenv-utf-8 "CCLSH_QUICKLISP_SETUP")))
    (if (and override (plusp (length override)))
        (pathname override)
        (merge-pathnames "quicklisp/setup.lisp"
                         (user-homedir-pathname)))))

(defun build-fail (control &rest arguments)
  "Report a build error clearly and leave without saving an image."
  (format *error-output* "cclsh build: error: ~?~%" control arguments)
  (finish-output *error-output*)
  (ccl:quit 1))

(defun build-quicklisp-relative-files ()
  "Return the strict allowlist of Quicklisp template files."
  (append
   *build-quicklisp-root-files*
   (mapcar (lambda (name) (concatenate 'string "quicklisp/" name))
           *build-quicklisp-client-files*)
   (mapcar (lambda (name)
             (concatenate 'string "dists/quicklisp/" name))
           *build-quicklisp-dist-files*)))

(defun build-materialize-quicklisp-template ()
  "Copy sanitized Quicklisp metadata into a private temporary home."
  (let* ((setup  (build-quicklisp-source-setup-pathname))
         (source (uiop:pathname-directory-pathname setup))
         (target
           (merge-pathnames
            (format nil ".cclsh-quicklisp-build.~d/" (ccl::getpid))
            (truename "./"))))
    (unless (probe-file setup)
      (build-fail
       "Quicklisp is required, but ~a does not exist.~%~
        Install it there, or set CCLSH_QUICKLISP_SETUP to its setup.lisp."
       setup))
    (when (probe-file target)
      (uiop:delete-directory-tree target
                                  :validate t
                                  :if-does-not-exist ':ignore))
    (dolist (relative (build-quicklisp-relative-files))
      (let ((source-file (merge-pathnames relative source))
            (target-file (merge-pathnames relative target)))
        (unless (probe-file source-file)
          (build-fail "Quicklisp template input is missing: ~a"
                      source-file))
        (ensure-directories-exist target-file)
        (uiop:copy-file source-file target-file)))
    (ensure-directories-exist (merge-pathnames "local-projects/" target))
    (setf *build-quicklisp-home* target)
    target))

(defun build-git-output (directory arguments)
  "Trimmed output of a Git command in DIRECTORY, or NIL on failure."
  (handler-case
      (let* ((output
               (make-string-output-stream))
             (process
               (ccl:run-program
                "git"
                (append (list "-C" (namestring directory)) arguments)
                :input           nil
                :output          output
                :error           nil
                :wait            t
                :external-format ':utf-8)))
        (multiple-value-bind (status code)
            (ccl:external-process-status process)
          (when (and (eq status ':exited) (zerop code))
            (string-trim '(#\newline #\return #\space #\tab)
                         (get-output-stream-string output)))))
    (error () nil)))

(defparameter *build-dependency-specifications*
  '((:name              "cl-colorist"
     :environment       "CCLSH_CL_COLORIST_SOURCE"
     :default-directory "../cl-colorist/"
     :asd               "cl-colorist.asd")
    (:name              "colorlisp"
     :environment       "CCLSH_COLORLISP_SOURCE"
     :default-directory "../colorlisp/"
     :asd               "colorlisp.asd")
    (:name              "colordiff"
     :environment       "CCLSH_COLORDIFF_SOURCE"
     :default-directory "../colordiff/"
     :asd               "colordiff.asd")
     (:name              "structlisp"
      :environment       "CCLSH_STRUCTLISP_SOURCE"
      :default-directory "../structlisp/"
      :asd               "structlisp.asd")
     (:name              "clinedi"
      :environment       "CCLSH_CLINEDI_SOURCE"
      :default-directory "../clinedi/"
      :asd               "clinedi.asd"))
  "Locked source dependencies loaded into the saved image, in load order.")

(defun build-lock-commit (name)
  "Return the exact commit recorded for dependency NAME."
  (handler-case
      (with-open-file (stream "dependencies.lock"
                              :direction       ':input
                              :external-format ':utf-8)
        (let ((commits '())
              (prefix  (concatenate 'string name "=")))
          (loop for line = (read-line stream nil nil)
                while line
                when (and (<= (length prefix) (length line))
                          (string= prefix line :end2 (length prefix)))
                  do (push (subseq line (length prefix)) commits))
          (let ((commit (and (null (rest commits)) (first commits))))
            (unless (and commit
                         (= (length commit) 40)
                         (every (lambda (character)
                                  (and (digit-char-p character 16)
                                       (not (find character "ABCDEF"))))
                                commit))
              (build-fail
               "dependencies.lock must contain exactly one ~a=<40-character lowercase Git commit> line"
               name))
            commit)))
    (error (condition)
      (build-fail "could not read dependencies.lock: ~a" condition))))

(defun build-load-quicklisp ()
  "Load sanitized Quicklisp metadata and verify its public entry point."
  (let* ((home  (build-materialize-quicklisp-template))
         (setup (merge-pathnames "setup.lisp" home)))
    (format t "Including sanitized Quicklisp metadata~%")
    (finish-output)
    (handler-case
        (load setup :verbose nil :external-format ':utf-8)
      (error (condition)
        (build-fail "could not load Quicklisp from ~a: ~a"
                    setup condition)))
    (let* ((package (find-package "QL"))
           (quickload (and package (find-symbol "QUICKLOAD" package))))
      (unless (and quickload (fboundp quickload))
        (build-fail
         "~a loaded without providing the function QL:QUICKLOAD"
         setup)))))

(build-load-quicklisp)

(defvar *build-dependency-identities* nil
  "Verified paths, repositories and commits of loaded source dependencies.")

(defun build-initialize-source-registry (identities)
  "Expose this checkout, locked dependencies and packaged project sources."
  (let* ((root (truename "./"))
         (projects (build-getenv-utf-8 "CCLSH_PROJECTS_SOURCE")))
    (asdf:initialize-source-registry
     `(:source-registry
       (:directory ,root)
       ,@(mapcar
          (lambda (identity)
            `(:directory
              ,(uiop:pathname-directory-pathname
                (getf identity ':asd))))
          identities)
       ,@(when (and projects (plusp (length projects)))
           `((:tree ,(uiop:ensure-directory-pathname projects))))
       :ignore-inherited-configuration)))
  (values))

(defun build-dependency-asd-pathname (specification)
  "Return the sibling or overridden ASD for SPECIFICATION."
  (let* ((name        (getf specification ':name))
         (environment (getf specification ':environment))
         (override    (build-getenv-utf-8 environment))
         (directory
           (if (and override (plusp (length override)))
               (uiop:ensure-directory-pathname (pathname override))
               (merge-pathnames
                (getf specification ':default-directory)
                (truename "./"))))
         (asd (merge-pathnames (getf specification ':asd) directory)))
    (unless (probe-file asd)
      (build-fail
       "~a checkout is missing at ~a.~%Set ~a to its repository directory."
       name directory environment))
    (truename asd)))

(defun build-dependency-identity (specification asd expected-commit)
  "Return the clean, locked Git identity containing ASD."
  (let* ((name      (getf specification ':name))
         (directory
           (uiop:pathname-directory-pathname asd))
         (root-output
           (build-git-output directory '("rev-parse" "--show-toplevel")))
         (commit
           (build-git-output directory
                             '("rev-parse" "--verify" "HEAD^{commit}")))
         (status
           (build-git-output directory
                             '("status" "--porcelain"
                               "--untracked-files=all"))))
    (unless (and root-output (plusp (length root-output)))
      (build-fail "the selected ~a system is not in a Git checkout: ~a"
                  name asd))
    (let* ((root         (truename (uiop:ensure-directory-pathname
                                    root-output)))
           (expected-asd
             (truename
              (merge-pathnames (getf specification ':asd) root))))
      (unless (equal asd expected-asd)
        (build-fail "selected ~a instead of repository-root ~a"
                    asd expected-asd))
      (unless (and commit (string= commit expected-commit))
        (build-fail
         "~a checkout ~a is at ~a, but dependencies.lock requires ~a"
         name root (or commit "an unreadable commit") expected-commit))
      (unless (and (stringp status) (zerop (length status)))
        (build-fail "~a checkout ~a must be clean; Git reports:~%~a"
                    name root (or status "could not read repository status")))
      (list :name          name
            :specification specification
            :asd           asd
            :root          root
            :commit        commit))))

(defun build-loaded-dependency-identity (name)
  "Return the verified identity for loaded dependency NAME."
  (or (find name *build-dependency-identities*
            :key (lambda (identity) (getf identity ':name))
            :test #'string=)
      (build-fail "no loaded identity exists for dependency ~a" name)))

(defun build-load-dependencies ()
  "Load every exact, clean source revision required by this build."
  (setf *build-dependency-identities*
        (mapcar
         (lambda (specification)
           (let ((asd
                   (build-dependency-asd-pathname specification)))
             (build-dependency-identity
              specification
              asd
              (build-lock-commit (getf specification ':name)))))
         *build-dependency-specifications*))
  (build-initialize-source-registry *build-dependency-identities*)
  (dolist (identity *build-dependency-identities*)
    (let* ((name (getf identity ':name))
           (asd  (getf identity ':asd)))
      (asdf:load-asd asd)
      (let ((selected
              (truename
               (asdf:system-source-file (asdf:find-system name)))))
        (unless (equal asd selected)
          (build-fail "ASDF selected ~a from ~a instead of ~a"
                      name selected asd)))
      ;; Do not let an older cached FASL stand in for locked source.
      (asdf:load-system name :force t)
      (format t "Including ~a ~a from ~a~%"
              name (getf identity ':commit) asd)
      (finish-output)))
  (values))

(defun build-verify-dependency-identities ()
  "Require every loaded dependency to retain its verified identity."
  (dolist (identity *build-dependency-identities*)
    (let* ((name          (getf identity ':name))
           (specification (getf identity ':specification))
           (expected      (getf identity ':commit))
           (asd           (getf identity ':asd))
           (current
             (build-dependency-identity specification asd expected))
           (selected
             (truename
              (asdf:system-source-file (asdf:find-system name)))))
      (unless (equal identity current)
        (build-fail "~a identity changed while cclsh was being built" name))
      (unless (equal asd selected)
        (build-fail "ASDF's selected ~a changed from ~a to ~a"
                    name asd selected))))
  (values))

(build-load-dependencies)

(defun build-load-cclsh ()
  "Load cclsh from this checkout after its locked dependency."
  (let ((asd (truename "cclsh.asd")))
    (asdf:load-asd asd)
    (let ((loaded (truename
                   (asdf:system-source-file
                    (asdf:find-system "cclsh")))))
      (unless (equal asd loaded)
        (build-fail "ASDF selected ~a instead of ~a" loaded asd)))
    (asdf:load-system "cclsh" :force t)))

(build-load-cclsh)
(build-verify-dependency-identities)

(defun build-read-file-octets (file)
  "Read FILE into a simple octet vector."
  (with-open-file (stream file
                          :direction ':input
                          :element-type '(unsigned-byte 8))
    (let* ((length   (file-length stream))
           (contents (make-array length
                                 :element-type '(unsigned-byte 8))))
      (unless (= length (read-sequence contents stream))
        (build-fail "could not read complete Quicklisp template file: ~a"
                    file))
      contents)))

(defun build-quicklisp-template-files ()
  "Return a sanitized Quicklisp template for embedding in the image."
  (let ((home (uiop:ensure-directory-pathname ql-setup:*quicklisp-home*)))
    (mapcar
     (lambda (relative)
       (let ((file (merge-pathnames relative home)))
         (unless (probe-file file)
           (build-fail "Quicklisp template input is missing: ~a" file))
         (list relative (build-read-file-octets file))))
     (build-quicklisp-relative-files))))

(defun build-git-commit ()
  "The short commit of the checkout, with a -dirty marker when the
   working tree has uncommitted changes. A packager may provide the
   immutable source identity through CCLSH_BUILD_COMMIT."
  (let ((packaged (build-getenv-utf-8 "CCLSH_BUILD_COMMIT")))
    (if (and packaged (plusp (length packaged)))
        packaged
        (let* ((root   (truename "./"))
               (commit (build-git-output
                        root '("rev-parse" "--short" "HEAD"))))
          (when (and commit (plusp (length commit)))
            (if (plusp (length (or (build-git-output
                                    root '("status" "--porcelain")) "")))
                (concatenate 'string commit "-dirty")
                commit))))))

(let* ((commit (build-git-commit))
       (clinedi-commit
         (getf (build-loaded-dependency-identity "clinedi") ':commit))
       (structlisp-commit
         (getf (build-loaded-dependency-identity "structlisp") ':commit))
       (cl-colorist-commit
         (getf (build-loaded-dependency-identity "cl-colorist") ':commit))
       (colorlisp-commit
         (getf (build-loaded-dependency-identity "colorlisp") ':commit))
       (colordiff-commit
         (getf (build-loaded-dependency-identity "colordiff") ':commit))
       (quicklisp-template
         (build-getenv-utf-8 "CCLSH_PACKAGED_QUICKLISP_TEMPLATE"))
       (quicklisp-files
         (unless (and quicklisp-template
                      (plusp (length quicklisp-template)))
           (build-quicklisp-template-files))))
  (setf cclsh:*cclsh-build-commit*             commit
        cclsh:*cclsh-build-clinedi-commit*     clinedi-commit
        cclsh:*cclsh-build-structlisp-commit*  structlisp-commit
        cclsh:*cclsh-build-cl-colorist-commit* cl-colorist-commit
        cclsh:*cclsh-build-colorlisp-commit*   colorlisp-commit
        cclsh:*cclsh-build-colordiff-commit*   colordiff-commit
        cclsh::*packaged-quicklisp-template*
        (and quicklisp-template
             (plusp (length quicklisp-template))
             quicklisp-template)
        cclsh::*packaged-quicklisp-files*           quicklisp-files
        cclsh::*packaged-quicklisp-home*            nil
        cclsh::*packaged-quicklisp-user-setup-home* nil
        cclsh::*packaged-quicklisp-last-error*      nil)
  (format t "Build commit: ~a~%" (or commit "unknown")))

;; Keep the heap image separate from the kernel so installers can activate
;; a matched, content-addressed pair atomically. scripts/build installs the
;; local pair only after both files exist and the saved image validates.
(build-verify-dependency-identities)
(dolist (system '("cclsh"
                  "clinedi"
                  "clinedi/tests"
                  "structlisp"
                  "structlisp/tests"
                  "cl-colorist"
                  "cl-colorist/tests"
                  "quicklisp"))
  (asdf:clear-system system))
(format t "Copying the CCL kernel...~%")
(uiop:copy-file (truename "/proc/self/exe") "cclsh.new")
(setf *build-dependency-identities* nil)
(when *build-quicklisp-home*
  (uiop:delete-directory-tree *build-quicklisp-home*
                              :validate t
                              :if-does-not-exist ':ignore)
  (setf *build-quicklisp-home* nil))

(in-package #:cl-user)

(delete-package '#:cclsh-build)
(ccl:gc)
(format t "Saving cclsh image...~%")
(ccl:save-application "cclsh.image.new"
                      :toplevel-function #'cclsh:shell-toplevel
                      :prepend-kernel nil
                      :mode #o600)
