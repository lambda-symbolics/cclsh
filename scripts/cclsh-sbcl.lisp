;;;; -- SBCL source launcher --
;;;
;;; This deliberately does not change the caller's working directory.  It
;;; only makes the checkout and its explicit sibling dependencies visible to
;;; ASDF, then transfers control to CCLSH's ordinary toplevel.

(require :asdf)

(defpackage #:cclsh-sbcl-launcher
  (:use #:cl))

(in-package #:cclsh-sbcl-launcher)

(defparameter *checkout-directory*
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-directory-pathname
    (or *load-truename* *compile-file-truename*)))
  "The CCLSH checkout containing this launcher.")

(defparameter *dependency-specifications*
  '(("clinedi" "CCLSH_CLINEDI_SOURCE" "../clinedi/" "clinedi.asd")
    ("structlisp" "CCLSH_STRUCTLISP_SOURCE" "../structlisp/" "structlisp.asd")
    ("cl-colorist" "CCLSH_CL_COLORIST_SOURCE" "../cl-colorist/"
     "cl-colorist.asd")
    ("colorlisp" "CCLSH_COLORLISP_SOURCE" "../colorlisp/" "colorlisp.asd")
    ("colordiff" "CCLSH_COLORDIFF_SOURCE" "../colordiff/" "colordiff.asd"))
  "Sibling source dependencies used by the source launcher.")

(defun launcher-fail (control &rest arguments)
  "Write a concise startup diagnostic and terminate SBCL unsuccessfully."
  (format *error-output* "cclsh-sbcl: ~?~%" control arguments)
  (finish-output *error-output*)
  (uiop:quit 1))

(defun call-with-muffled-warnings (function)
  "Run FUNCTION without Quicklisp and ASDF startup noise."
  (let ((quiet-output (make-broadcast-stream)))
    (let ((*standard-output* quiet-output)
          (*trace-output* quiet-output)
          (*compile-verbose* nil)
          (*compile-print* nil)
          (*load-verbose* nil)
          (*load-print* nil))
      (handler-bind
          ((warning
             (lambda (condition)
               (declare (ignore condition))
               (let ((restart (find-restart 'muffle-warning)))
                 (when restart (invoke-restart restart))))))
        (funcall function)))))

(defun quicklisp-setup-pathname ()
  "Return the configured Quicklisp setup file pathname."
  (let ((override (uiop:getenv "CCLSH_QUICKLISP_SETUP")))
    (if (and override (plusp (length override)))
        (pathname override)
        (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))))

(defun load-quicklisp ()
  "Load Quicklisp and return its QUICKLOAD function."
  (let ((setup (quicklisp-setup-pathname)))
    (unless (probe-file setup)
      (launcher-fail
       "Quicklisp is required; install it or set CCLSH_QUICKLISP_SETUP (missing ~a)"
       setup))
    (call-with-muffled-warnings (lambda () (load setup :verbose nil)))
    (let* ((package (find-package "QL"))
           (quickload (and package (find-symbol "QUICKLOAD" package))))
      (unless (and quickload (fboundp quickload))
        (launcher-fail "~a did not provide QL:QUICKLOAD" setup))
      quickload)))

(defun dependency-asd-pathname (specification)
  "Return SPECIFICATION's ASD pathname, checking its override or sibling."
  (destructuring-bind (name environment default asd-name) specification
    (let* ((override (uiop:getenv environment))
           (directory
             (if (and override (plusp (length override)))
                 (uiop:ensure-directory-pathname (pathname override))
                 (merge-pathnames default *checkout-directory*)))
           (asd (merge-pathnames asd-name directory)))
      (unless (probe-file asd)
        (launcher-fail
         "~a is missing at ~a; clone it beside cclsh or set ~a"
         name directory environment))
      (truename asd))))

(defun initialize-source-registry ()
  "Prioritize this checkout and its declared source dependencies."
  (let ((dependencies
          (mapcar #'dependency-asd-pathname *dependency-specifications*)))
    (asdf:initialize-source-registry
     `(:source-registry
       (:directory ,*checkout-directory*)
       ,@(mapcar (lambda (asd)
                   `(:directory ,(uiop:pathname-directory-pathname asd)))
                 dependencies)
       :ignore-inherited-configuration))
    dependencies))

(defun ensure-selected-system (name asd)
  "Load NAME's ASD and require ASDF to select exactly ASD."
  (asdf:load-asd asd)
  (let ((selected (asdf:system-source-file (asdf:find-system name))))
    (unless (and selected (equal (truename selected) (truename asd)))
      (launcher-fail "ASDF selected ~a for ~a instead of ~a"
                     selected name asd))))

(let ((quickload (load-quicklisp))
      (dependencies (initialize-source-registry))
      (cclsh-asd (truename (merge-pathnames "cclsh.asd" *checkout-directory*))))
  ;; These three host libraries provide the SBCL terminal, threading and Gray
  ;; stream layers. Quicklisp downloads them on first use when necessary.
  (call-with-muffled-warnings
   (lambda ()
     (funcall quickload '("cffi" "bordeaux-threads" "trivial-gray-streams")
              :silent t)))
  (call-with-muffled-warnings
   (lambda ()
     (dolist (specification *dependency-specifications*)
       (ensure-selected-system (first specification) (pop dependencies)))
     (ensure-selected-system "cclsh" cclsh-asd)
     (asdf:load-system "cclsh"))))

(cclsh:shell-toplevel)
