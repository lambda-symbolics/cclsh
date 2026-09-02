;;;; -- Line editor adapter --

(in-package #:cclsh)

(defparameter *line-editor-keymap* (clinedi:default-line-editor-keymap)
  "Mutable Clinedi event-to-command keymap used for interactive input.
Startup files may replace this with another CLINEDI:KEYMAP or change bindings
with CLINEDI:KEYMAP-BIND. The default map is private to this CCLSH image.")

(defparameter *line-editor-word-delimiter-mode-p* t
  "Whether Ctrl-word movement and deletion split at word delimiters.

CCLSH enables Clinedi's delimiter-aware word editing by default.")

(defparameter *line-editor-word-delimiters*
  (copy-list clinedi:*default-word-delimiters*)
  "Characters that delimit Ctrl-word movement and deletion.

The default is Clinedi's hyphen, underscore, slash, dot, and colon set.")


(defun line-editor--accept-completion (candidate)
  "Return the accepted form of CANDIDATE for insertion into shell input.
   Directory candidates stay open for further path completion. Other unique
   candidates receive the separating space expected by shell input."
  (if (and (plusp (length candidate))
           (char= (char candidate (1- (length candidate))) #\/))
      candidate
      (concatenate 'string candidate " ")))

(defun edit-line (prompt &key (history *history*) semantic-prompt-p)
  "Edit one shell input line under PROMPT.
   Return the line and a result kind of :LINE, :ABORT or :EOF."
  (clinedi:edit-line
   prompt
   :history history
   :history-match-function #'history-search-match-p
   :terminal-size-function #'terminal-size
   :raw-mode-function #'terminal-raw
   :restore-function #'terminal-restore
   :highlight-function #'highlight-line
   :completion-function #'complete-line
   :common-prefix-function #'completion--common-prefix
   :completion-accept-function #'line-editor--accept-completion
   :completion-arrangement :grid
   :suggestion-function #'history-suggestion
   :word-delimiter-mode-p *line-editor-word-delimiter-mode-p*
   :word-delimiters *line-editor-word-delimiters*
   :before-prompt-function
   (and semantic-prompt-p
        (lambda (stream)
          (terminal-write-semantic-marker ':prompt-start 0 stream)))
   :after-prompt-function
   (and semantic-prompt-p
        (lambda (stream)
          (terminal-write-semantic-marker ':input-start 0 stream)))
   :keymap *line-editor-keymap*))
