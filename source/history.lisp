;;;; -- History persistence --
;;;
;;; History entries are stored one PRIN1ed string per entry, so entries
;;; containing newlines or quotes survive round-trips and loading is a
;;; plain READ loop with *READ-EVAL* disabled.

(in-package #:cclsh)

(defvar *history* (make-array 0 :adjustable t :fill-pointer t)
  "In-memory command history, oldest entry first.")

(defvar *history-limit* 10000
  "Maximum number of history entries kept on load.")

(defun history--immediate-duplicate-p (entry history)
  "True when ENTRY repeats HISTORY's newest entry exactly."
  (and (plusp (fill-pointer history))
       (string= entry (aref history (1- (fill-pointer history))))))

(defun config-directory ()
  "Return the cclsh configuration directory as a string with a trailing
   slash, honoring XDG_CONFIG_HOME."
  (let* ((xdg  (getenv "XDG_CONFIG_HOME"))
         (base (if (and xdg (plusp (length xdg)))
                   (if (char= (char xdg (1- (length xdg))) #\/)
                       xdg
                       (concatenate 'string xdg "/"))
                   (namestring (merge-pathnames ".config/"
                                                (user-homedir-pathname))))))
    (concatenate 'string base "cclsh/")))

(defun history-file ()
  "Return the path of the persistent history file."
  (concatenate 'string (config-directory) "history"))

(defun history-suggestion (input &optional (history *history*))
  "Newest entry in HISTORY that starts with nonempty INPUT and has
   more text to suggest. Equal entries are skipped so an older, longer
   command can still match."
  (when (plusp (length input))
    (loop for index downfrom (1- (fill-pointer history)) to 0
          for entry = (aref history index)
          when (and (> (length entry) (length input))
                    (string= input entry :end2 (length input)))
            return entry)))

(defun history-search-match-p (query entry)
  "True when ENTRY contains QUERY using fish-style smart case.
   Lowercase queries ignore case; an uppercase character makes the whole
   query case-sensitive."
  (not (null (search query entry
                     :test (if (find-if #'upper-case-p query)
                               #'char=
                               #'char-equal)))))

(defun history-load ()
  "Load persisted history into *HISTORY*. Unreadable content is
   silently ignored; only the newest *HISTORY-LIMIT* entries are kept.
   Loading is bounded, so a large history file does not accumulate an
   unbounded intermediate list."
  (setf (fill-pointer *history*) 0)
  (let ((entries (structlisp:make-deque
                  :maximum-count *history-limit*)))
    (handler-case
        (with-open-file (stream (history-file)
                                :direction :input
                                :if-does-not-exist nil
                                :external-format ':utf-8)
          (when stream
            (let ((*read-eval* nil))
              (loop for entry = (read stream nil ':cclsh-eof)
                    until (eq entry ':cclsh-eof)
                    when (stringp entry)
                      do (structlisp:deque-push-back entries entry)))))
      (error () nil))
    ;; Older history files can contain repeats from earlier cclsh releases or
    ;; concurrent shells. Keep nonconsecutive repeats, but collapse a run so
    ;; partial history traversal never makes the user walk the same command.
    (loop for entry across (structlisp:deque->vector entries)
          unless (history--immediate-duplicate-p entry *history*)
            do (vector-push-extend entry *history*)))
  (values))

(defun history-append (entry)
  "Record ENTRY in memory and append it to the history file. Blank
   entries and immediate duplicates are skipped."
  (let ((trimmed (string-trim *whitespace-characters* entry)))
    (when (and (plusp (length trimmed))
               (not (history--immediate-duplicate-p entry *history*)))
      (vector-push-extend entry *history*)
      (handler-case
          (progn
            (ensure-directories-exist (config-directory))
            (path-set-mode (config-directory) #o700)
            (let ((descriptor
                    (fd-open-output (history-file)
                                    :append t
                                    :mode #o600)))
              (unwind-protect
                  (progn
                    (fd-set-mode descriptor #o600)
                    (let ((stream (fd-output-stream descriptor)))
                      (setf descriptor nil)
                      (with-open-stream (stream stream)
                        (prin1 entry stream)
                        (terpri stream))))
                (when descriptor
                  (fd-close descriptor)))))
        (error () nil))))
  (values))
