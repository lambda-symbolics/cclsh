(defsystem "cclsh"
  :version "1.5.1"
  :author "Lukáš Hozda"
  :license "ISC"
  :encoding :utf-8
  :depends-on ("clinedi" "colordiff" "structlisp"
               #+sbcl "bordeaux-threads"
               #+sbcl "trivial-gray-streams"
               #+sbcl "cffi")
  :components ((:module "source"
                :components
                 (#+sbcl (:file "sbcl/compat")
                 (:file "package" :depends-on (#+sbcl "sbcl/compat"))
                 #+ccl (:file "environment" :depends-on ("package"))
                 #+sbcl (:file "environment" :pathname "sbcl/environment"
                          :depends-on ("package"))
                 #+ccl (:file "prewarm" :depends-on ("package" "environment"))
                 #+sbcl (:file "prewarm" :pathname "sbcl/prewarm"
                          :depends-on ("package" "environment"))
                 #+ccl (:file "terminal" :depends-on ("package"))
                 #+sbcl (:file "terminal" :pathname "sbcl/terminal"
                          :depends-on ("package"))
                 #+ccl (:file "process" :depends-on ("package" "environment"))
                 #+sbcl (:file "process" :pathname "sbcl/process"
                          :depends-on ("package" "environment"))
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
  :description "A system shell running inside Common Lisp")
