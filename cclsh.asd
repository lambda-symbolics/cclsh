(defsystem "cclsh"
  :version "1.4.0"
  :author "Lukáš Hozda"
  :license "ISC"
  :encoding :utf-8
  :depends-on ("clinedi" "colordiff" "structlisp")
  :components ((:module "source"
                :components
                ((:file "package")
                 (:file "environment" :depends-on ("package"))
                 (:file "prewarm" :depends-on ("package" "environment"))
                 (:file "terminal" :depends-on ("package"))
                 (:file "process" :depends-on ("package" "environment"))
                 (:file "lexer"    :depends-on ("package"))
                 (:file "command"  :depends-on ("package" "environment" "lexer"))
                 (:file "jobs"     :depends-on ("command" "terminal" "process"))
                 (:file "expand"   :depends-on ("lexer" "command" "environment"))
                 (:file "highlight" :depends-on ("terminal" "lexer" "command" "expand"))
                 (:file "history"
                  :depends-on ("lexer" "environment" "process"))
                 (:file "prompt"   :depends-on ("terminal" "command" "expand" "environment" "jobs"))
                 (:file "pipeline" :depends-on ("command" "jobs"))
                 (:file "directory"
                  :depends-on ("command" "jobs" "expand" "terminal" "pipeline"))
                 (:file "complete" :depends-on ("lexer" "command" "expand" "highlight"))
                 (:file "edit" :depends-on ("command" "jobs" "expand" "environment"))
                 (:file "builtins" :depends-on ("command" "jobs" "expand" "terminal" "history" "complete"))
                 (:file "manual"   :depends-on ("command" "terminal"))
                 (:file "line-editor" :depends-on ("terminal" "highlight" "history" "complete"))
                 (:file "dispatch"
                  :depends-on ("command" "jobs" "expand" "lexer" "highlight"
                               "complete" "directory" "builtins"))
                 (:file "main"
                  :depends-on ("dispatch" "line-editor" "prompt" "directory"
                               "builtins" "pipeline" "jobs" "manual"
                               "prewarm")))))
  :description "A system shell running inside Clozure CL")
