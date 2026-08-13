;;;; -- Package definitions --

(defpackage #:cclsh
  (:use #:cl)
  (:import-from #:ccl
                #:run-program
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
                #:*command-line-argument-list*)
  (:import-from #:clinedi
                #:*presentation-enabled*
                #:ansi-colorize
                #:ansi-reverse-video
                #:ansi-cursor-up
                #:ansi-cursor-column
                #:ansi-cursor-hide
                #:ansi-cursor-show
                #:ansi-clear-below
                #:ansi-clear-line-right
                #:ansi-clear-screen
                #:ansi-strip
                #:ansi-display-width)
  (:shadow #:export)
  (:export #:main
           #:shell-toplevel
           #:*cclsh-version*
           #:*cclsh-build-commit*
           #:*cclsh-build-clinedi-commit*
           #:*cclsh-build-structlisp-commit*
           #:*cclsh-build-cl-colorist-commit*
             #:*cclsh-build-colorlisp-commit*
             #:*cclsh-build-colordiff-commit*
           #:*argv*
           #:*directory-change-hooks*
           #:*prompt-function*
           #:*line-editor-keymap*
           #:prompt-default
           #:command
           #:command-name
           #:command-function
           #:command-documentation
           #:command-arguments
           #:command-declarative-arguments-p
           #:command-argument
           #:command-argument-name
           #:command-argument-kind
           #:command-argument-value-type
           #:command-argument-required-p
           #:command-argument-repeating-p
           #:command-argument-short-name
           #:command-argument-long-name
           #:command-argument-documentation
           #:command-argument-choices
           #:command-argument-completion-function
           #:command-argument-metavariable
           #:command-argument-convert-p
           #:command-argument-converter
           #:command-argument-choice-values
           #:command-completion-context
           #:make-command-completion-context
           #:command-completion-context-command
           #:command-completion-context-argument
           #:command-completion-context-words
           #:command-completion-context-used-options
           #:command-completion-context-positional-index
           #:command-completion-context-options-enabled-p
           #:command-completion-context-prefix
           #:command-completion-context-buffer
           #:command-completion-context-cursor
           #:completion-candidate
           #:make-completion-candidate
           #:completion-candidate-p
           #:completion-candidate-insertion
           #:completion-candidate-display
           #:completion-candidate-description
           #:completion-candidate-kind
           #:command-arguments-context
           #:command-arguments-parse
           #:command-help-string
           #:command-print-help
           #:defcommand
           #:run
           #:cmd
           #:pipe
           #:capture
           #:glob
           #:seq
           #:all
           #:any
           #:*last-status*
           #:cd
           #:directory-change-hook-add
           #:directory-change-hook-remove
           #:zoxide-setup
           #:z
           #:zi
           #:exit
           #:rehash
             #:commands
             #:help
             #:edit
             #:jobs
             #:disown
           #:fg
           #:bg
           #:export
           #:unset
           #:getenv
           #:setenv
           #:env
           #:environment-variables
           #:config-directory
           #:quicklisp-setup
           #:shell-error
           #:command-not-found-error
           #:command-argument-error
           #:pipeline-syntax-error)
  (:documentation "A system shell running inside Clozure CL."))

(defpackage #:cclsh-user
  (:use #:cl #:ccl #:cclsh)
  (:shadowing-import-from #:cclsh
                          #:run
                          #:cmd
                          #:pipe
                          #:capture
                          #:seq
                          #:all
                          #:any
                          #:cd
                          #:exit
                           #:rehash
                           #:commands
                           #:help
                           #:edit
                           #:jobs
                           #:disown
                          #:fg
                          #:bg
                          #:export
                          #:unset
                          #:getenv
                          #:setenv
                          #:command
                          #:defcommand)
  (:documentation "The package command lines and startup.lisp are evaluated in."))

(in-package #:cclsh)

(defvar *argv* nil
  "Argument vector of the running cclsh script, or NIL outside script mode.
The first element is the script path and the remaining elements are the
arguments supplied after it.")
