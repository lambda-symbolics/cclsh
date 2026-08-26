;;;; -- Manual --
;;;
;;; The built-in manual behind the HELP command. Content lives here as
;;; data so the installed binary is self-documenting; docs/guide.org
;;; is the long form. Everything not covered here is ordinary Common
;;; Lisp in a live CCL image.

(in-package #:cclsh)

(defparameter *manual-sections*
  '(("dispatch" "how a line becomes Lisp or a command"
     "A line starting with ( evaluates as Lisp in the cclsh-user
package. Anything else is a command line whose first word resolves in
order:

  1. an existing non-directory word containing / runs directly
  2. a symbol whose value is a COMMAND instance runs as a builtin
  3. the word is looked up in PATH
  4. one explicit directory path changes directory as if by cd
  5. a lone word naming a bound variable or keyword, or a number
     literal, evaluates REPL style
  6. otherwise: command not found, status 127

A line whose first non-whitespace character is ; is a comment. The
whole submitted line is ignored and the previous status is preserved.
Semicolons elsewhere in command lines remain literal arguments.

  ./configure              a path, runs directly
  cd src                   cd is bound to a COMMAND, a builtin
  ls -la                   found in PATH
  ..                       changes to the parent directory
  src/                     changes to the src directory
  (+ 1 2)                  starts with (, evaluates as Lisp
  *last-status*            lone bound variable, prints its value
  pi                       3.141592653589793D0
  42                       numbers print themselves

Implicit cd accepts .., absolute paths, paths beginning ./ or ../,
expanded ~/ paths and names ending in /. A bare name remains a command
lookup. The path must be the only word and cannot be backgrounded.
Builtins and executables always win first.

PATH lookups are cached for highlighting, but execution retries a
cached miss with a fresh scan, so freshly installed programs work
immediately. rehash drops all caches.")

    ("expansion" "words, quoting, variables and globs"
     "Command lines split on whitespace. In bare words:

  ~ and ~/x      home directory, only at the start of a word
  $VAR ${VAR}    environment variables, empty when unset
  * and ?        globs, matched per path segment
  \\x             escape: \\  joins words, \\* is a literal star

  echo ~/notes             /home/mag/notes
  echo $HOME/bin           /home/mag/bin
  echo *.lisp              main.lisp util.lisp
  echo *.zip               *.zip when nothing matches
  echo .h*                 .hidden, dotfiles need the explicit dot
  echo \"a b\" 'c d'         two words: a b and c d
  echo \"v=$HOME\" '$HOME'   variables expand in double quotes only
  echo pre\"mid dle\"post    one word: premid dlepost
  echo up\\ time            one word: up time

There is no | > or < syntax; a literal | is just an argument. # never
starts a command comment, and ; only does at the start of a submitted
line after whitespace. Pipelines are Lisp, see pipelines.")

    ("substitution" "Lisp values inside command lines"
     "Parens inside a command line substitute Lisp values. The outer
parens are shell delimiters: one form inside is an expression, several
forms become one call.

  echo (*balls*)                    HI, the value of *balls*
  echo (+ 1 2)                      3, several forms are a call
  echo (string-downcase *balls*)    hi
  echo (list 1 2 3)                 1 2 3, proper lists splice
  echo (getenv 'missing)            nothing at all, NIL vanishes
  echo x(+ 1 2)y                    x3y, in-word concatenation
  cd (*project-directory*)          anywhere a word can go
  mv draft.txt (format nil \"post-~a.txt\" (get-universal-time))

$(form) is the same thing. Results never re-glob or re-expand. Quote
parens to keep them literal: '(1 2 3)' stays text, which also means
filenames containing parens need quoting.")

    ("lisp" "Lisp lines and the REPL side"
     "Lisp lines evaluate in cclsh-user (uses cl, ccl and cclsh) and
print their values one per line.

  (+ 1 2)                  3
  (* 2 *)                  6, * holds the last value (** and *** too)
  (defvar *project* \"~/src\")
  cd (*project*)           definitions feed straight back into commands
  (ql:quickload :dexador)  every saved image includes Quicklisp

  (defparam x 1)           cclsh: undefined function defparam,
                           did you mean defparameter?

Undefined calls are caught before their arguments run. Unbalanced
forms, open strings and trailing backslashes continue on the next line
under a ... prompt. *last-status* holds the last exit status. Errors
print red and return to the prompt; the CCL debugger is never entered.
Output missing a final newline gets a reverse video ⏎ marker, fish
style:

  (format t \"hello\")       hello⏎ then NIL on a fresh line")

    ("unicode" "UTF-8 text and byte-transparent files"
     "Terminal I/O, startup and history files, scripts, prompt output,
environment names and values, child arguments and paths, and captured
pipeline text are UTF-8 regardless of the process locale.

The editor measures terminal cells. Wide CJK and emoji glyphs wrap in
two cells; combining marks and joined emoji move and delete as one
grapheme.

Redirect-only pipelines are byte-transparent, so arbitrary binary
files can be copied without UTF-8 decoding:

  (pipe (from \"input.bin\") (to \"output.bin\"))")

    ("commands" "defining commands, run and cmd"
     "defcommand defines a shell command that is also an ordinary
function:

  (defcommand gs ()
    \"Short git status.\"
    (run \"git\" \"status\" \"--short\"))

  (defcommand mkcd (directory)
    \"Create DIRECTORY and enter it.\"
    (:arguments
      (directory :type :directory :help \"Directory to create.\"))
    (run \"mkdir\" \"-p\" directory)
    (cd directory))

  gs                       from the command line
  (gs)                     and from Lisp
  (mkcd \"/tmp/scratch\")

An :arguments declaration makes the lambda list self-describing.
Required and optional parameters are positional, &rest repeats, and
&key parameters become long options:

  (defcommand deploy (source &key force jobs)
    \"Deploy SOURCE.\"
    (:arguments
      (source :type :directory :help \"Source tree.\")
      (force :type :boolean :short #\\f :help \"Replace output.\")
      (jobs :type :integer :convert t :short #\\j
            :help \"Worker count.\"))
    ...)

  deploy src -f -j4
  deploy --help
  help deploy

Metadata drives option parsing, generated help and semantic Tab
completion. :convert t opts into typed values; without it types are
advisory and values remain strings. :choices accepts a list or dynamic
function, and :completion installs a custom provider. Providers may return
simple values with optional descriptions, or completion-candidate records
with shell-ready insertion, display, description and semantic kind fields.
Existing commands without a declaration retain raw string arguments. Unknown
or malformed declared arguments return status 2 without entering the command
body.

An integer return value becomes the exit status, anything else means
0. Three ways to run programs from Lisp:

  (run \"git\" \"status\")     function, name may be computed
  (run 'git \"log\" \"-1\")    symbols work too
  (cmd git \"diff\" file)    macro, head resolves like a command word

Normally run creates its own foreground job. When a defcommand calls
run synchronously as a pipe or capture stage, run dynamically inherits
that stage's input, output and error and joins the surrounding pipeline
job. External-program wrappers therefore preserve piping, redirection,
Ctrl-C, Ctrl-Z and fg. Direct CCL:RUN-PROGRAM calls and separately
created Lisp threads bypass this inheritance.

Builtins: cd (with -), exit, export, unset, rehash, commands, help,
edit, jobs, disown, fg, bg and zoxide-setup. Saved cclsh images include
Quicklisp; quicklisp-setup loads or installs it when running from an unsaved
development image. commands lists everything currently defined.

edit NAME opens the recorded function definition in a temporary UTF-8 Lisp
file using VISUAL, EDITOR, or vi, in that order. After a successful editor
exit it displays a Colordiff before-and-after view with Lisp syntax
highlighting, evaluates the edited form in the function's package, and
installs the new definition in the running image. The original source file is
not changed, and the edited text remains available to later edit calls. From
Lisp, NAME may be a function object or symbol. CCL's native source notes
provide the definition; no SB-INTROSPECT is needed.")

    ("pipelines" "pipe, capture, glob, seq, all and any"
     "Process orchestration is spelled in Lisp. A stage is (name
argument...) where NAME resolves like a command word and the arguments
are evaluated expressions. A scalar becomes one string, NIL vanishes,
and a proper list splices one level. Strings stay literal, so quote
flags and use glob for explicit filesystem expansion:

  (pipe (ls \"-la\") (grep \"lisp\"))     ls -la | grep lisp
  (pipe (ls \"-1\" (glob \"screenshot-*\")) (wc \"-l\"))
  (seq (make \"clean\") (make))          make clean ; make
  (all (make) (make \"install\"))        make && make install
  (any (probe) (echo \"fallback\"))      probe || echo fallback

  (let ((pattern \"defun\"))
    (pipe (git \"grep\" pattern) (wc \"-l\")))

  (defcommand emit () (format t \"b~%a~%\") 0)
  (pipe (emit) (sort))                 builtins can sit in pipes

A synchronous run inside a builtin stage inherits that stage's
standard streams and pipeline job. This makes defcommand wrappers
around external programs compose with pipe, capture and redirection.

glob applies the same ~, environment-variable, * and ? expansion as a
bare command word and returns the matches as an ordinary list. Matches
are sorted within each pattern, pattern order is retained, and an
unmatched pattern remains literal. A glob result can be stored or
filtered before a stage splices it. run does not splice lists; spread
one explicitly with apply:

  (apply #'run \"ls\" \"-1\" (glob \"screenshot-*\"))

Redirection is spelled as stages, and capture returns output as a
string (sh's $(cmd)), trailing newlines trimmed:

  (pipe (make) (to \"build.log\"))       make > build.log
  (pipe (make) (append-to \"build.log\"))  make >> build.log
  (pipe (from \"in.txt\") (wc \"-l\"))     wc -l < in.txt
  (pipe (from \"a.bin\") (to \"b.bin\"))   byte-exact file copy
  (pipe (make) (error-to \"err.log\"))   make 2> err.log
  (pipe (make) (error-append-to \"err.log\"))  make 2>> err.log
  (capture (make) (merge-error))       both streams, sh's 2>&1
  (capture (git \"rev-parse\" \"HEAD\"))   \"3e45271...\"
  echo (capture (hostname))            capture inside substitution

Error redirection applies to every stage, builtins included, and
merge-error sends standard error wherever the ordinary output goes.
Each pipeline returns the deciding exit status and records
*last-status*. The pipeline owns the terminal as one job, so Ctrl-C
interrupts every stage and Ctrl-Z stops the whole pipeline together,
see jobs.")

    ("jobs" "background jobs, disown, fg, bg and Ctrl-Z"
     "Background and stopped commands are jobs:

  sleep 300 &              background job, prints [1] 12345
  make build               then Ctrl-Z stops it:
                           [2]+  Stopped                 make build
  jobs                     list jobs: + current, - previous;
                           jobs -l adds process group ids
  fg                       resume the current job in the foreground
  bg %2                    resume a stopped job in the background
  fg %ma                   specs: %2, 2, %+, %-, %prefix
  disown build             forget a job by id or command substring

 Disown removes a job from shell management without signaling it. The job
 is no longer listed and receives no SIGHUP from cclsh on exit.

Finished background jobs are announced before the next prompt as
Done, Exit 2 or the ending signal. fg restores the terminal modes a
stopped job was using, so Ctrl-Z out of vim and back just works.
Ctrl-Z stops a whole pipeline as one job; a capture continues through
Ctrl-Z with a notice, since the shell itself is reading its output.

Standalone builtins and Lisp forms run inside the shell process, so &
reports an error for them. Builtin stages inside pipe are controlled
with the rest of their pipeline and can be stopped and resumed. Ctrl-Z
during another busy Lisp evaluation stops the shell itself, so avoid
that one. exit with stopped jobs warns once, exit again to leave anyway.
Every orderly exit sends SIGHUP to tracked live job groups and SIGCONT to
tracked stopped groups so they do not linger after logout.
jobs, disown, fg and bg are ordinary functions too: (fg 1) resumes job 1
and (disown \"build\") forgets a matching job from Lisp.")

    ("editing" "keys, completion and colors"
     "At a slash-free command position, Tab completes command names and
directories. For a declared command it completes options with descriptions
and uses the active argument's path, directory, command, package, environment,
job, choice or custom provider. It understands option values, grouped short flags,
--long=value and --. Undeclared commands retain generic file completion.
Lisp symbols complete in Lisp mode or inside a substitution. Directories end
with /, making command-position matches ready for implicit cd, and
special characters get escaped:

  git ch<Tab>              checkout and friends
  ls src/ma<Tab>           src/main.lisp
  (princ-to<Tab>           (princ-to-string
  echo (*bal<Tab>          echo (*balls*

Unique matches insert and several extend to the common prefix. Tab again
opens a candidate grid: arrows navigate, Tab cycles, Escape restores the
prefix, and typing keeps the selected candidate before inserting. The newest
history entry beginning with the current input appears in dim text; Right or
C-f at the end accepts it. Separately, Up or C-p with text entered searches
older history entries containing the complete draft. Lowercase searches ignore
case; any uppercase letter makes the search case-sensitive. Down or C-n walks
newer matches and then restores the exact draft and cursor.

The built-in prompt shows username@hostname (PACKAGE) directory $. Set
*prompt-function* in startup.lisp to a function designator for another prompt.
It receives :status, :duration-milliseconds, :columns and :job-count keyword
arguments. A string is used verbatim; NIL selects prompt-default. Errors and
other values are reported and safely fall back. Use &allow-other-keys in custom
functions so future context additions remain compatible.

Interactive terminals receive OSC 133 semantic prompt markers for prompt start,
input start, execution start and command completion with the exit status. Set
*semantic-prompt-markers-enabled* to NIL in startup.lisp to disable them.

  Left/Right C-b/C-f   move/accept   Ctrl-arrows move by word
  Up/Down C-p/C-n      history match C-w/C-h     kill word
  Home/End C-a/C-e     line ends     C-k/C-u     kill rest/line
  Backspace / Delete   delete        C-l/C-c     clear/abort
  C-d                  delete forward, or exit on an empty line
  Alt/Ctrl-Enter       insert a newline without submitting
  Shift-Enter          same with CSI-u or modifyOtherKeys support

Clinedi key bindings are programmable from startup.lisp through
*line-editor-keymap*. Events and built-in commands use semantic keywords:

  (clinedi:keymap-bind *line-editor-keymap* :word-right :right)

Use clinedi:make-keymap with :parent for a layer or copy-keymap for a
detached map. The CCLSH default is private to this image.

Colors: external commands green, builtins and valid implicit directory
paths cyan, unknown red, lone bound variables magenta, strings yellow,
numbers cyan, $VAR magenta, globs and ~ bright magenta. In Lisp: known
operators in head position blue (a typo never lights up), keywords
magenta, constants cyan, bound *specials* magenta.")

    ("history" "what is remembered and where"
     "History persists in ~/.config/cclsh/history (XDG_CONFIG_HOME is
respected), one printed string per entry:

  \"ls -la\"
  \"(pipe (git \\\"log\\\") (head))\"

Loading keeps the newest 10000. Blank lines, aborted lines and
immediate duplicates are skipped. Non-interactive sessions neither
load nor write history. Multi-line entries recall with their original
newlines. With text entered, Up searches entries containing that complete
draft using smart case; Down walks newer matches and restores the draft after
the newest. Empty input traverses all history. The newest entry beginning with
current input is offered separately as a dim suggestion; Right or C-f accepts
it. Writes keep the cclsh
configuration directory mode at 0700 and the history file at 0600.")

    ("environment" "environment variables, the lispy way"
     "From the command line:

  export EDITOR=vim PAGER=less    set, several at a time
  export PATH=$HOME/bin:$PATH     expansions work in values
  export EDITOR                   print one value
  export                          print the whole environment
  unset PAGER

From Lisp, names are designators and values stringify:

  (setenv 'editor \"vim\")          (getenv :home)
  (setf (env 'port) 8000)         (env 'port) is \"8000\"
  (unset 'port)
  (environment-variables)         live NAME=value list

Lowercase names like http_proxy need strings. ~ does not expand after
=, use $HOME in values.

CCLSH_PACKAGE is maintained automatically as the canonical name of the
current Lisp package. It is refreshed before the configured prompt renderer
and for external commands launched through cclsh, including after
(in-package ...). The built-in prompt displays it directly. CCL:RUN-PROGRAM
calls made elsewhere bypass the refresh performed at those boundaries.")

    ("startup" "configuration and safe mode"
     "~/.config/cclsh/startup.lisp loads for interactive sessions and
configured command strings in cclsh-user:

  (setenv 'editor \"vim\")
  (defcommand la (&rest arguments)
    \"Long listing including hidden files.\"
    (apply #'run \"ls\" \"-la\" arguments))
  (clinedi:keymap-bind *line-editor-keymap* :word-right :right)
  (defvar *project* \"~/common-lisp/cclsh\")
  (zoxide-setup)

The run wrapper above remains pipeline-safe: when la is a pipe or
capture stage, its child inherits the stage's streams and job control.

A broken startup file prints its error and the shell starts anyway.
CCLSH_SAFE=1 skips startup.lisp and history entirely, the escape hatch
when user state misbehaves. The repository's examples/startup.lisp is
a sanitized login-safe starting point.")

    ("directories" "directory hooks and zoxide"
     "Register a function to observe every successful directory change:

  (defun announce-directory (old new)
    (format t \"moved from ~a to ~a~%\" old new))
  (directory-change-hook-add 'announce-directory)
  (directory-change-hook-remove 'announce-directory)

Hooks run after PWD, OLDPWD and CCL's default directory are committed.
A failing hook is reported without undoing cd or skipping later hooks. Use
symbols for named hooks so redefinition retains identity. A hook cannot call
cd again; reentrant changes are rejected to keep later hook state coherent.

Install zoxide, and fzf for interactive selection, then call this once
from startup.lisp:

  (zoxide-setup)

After zoxide records the current directory successfully, setup installs its
change hook, z and zi. z with no arguments goes home, z - goes to OLDPWD, an
existing path is entered directly and other arguments query zoxide. zi runs
zoxide's interactive query. Running setup after zoxide disappears removes
its stale hook and command bindings.")

    ("prewarm" "optional prewarmed interactive launcher"
     "cclsh-fast is an experimental, opt-in launcher that keeps one
one-shot CCLSH worker loaded ahead of time:

  cclsh-fast daemon start
  cclsh-fast daemon status
  cclsh-fast
  cclsh-fast daemon stop

Only a no-argument interactive session on one foreground terminal is eligible.
Options, scripts, pipes, redirections, login-style invocations and incompatible
process contexts execute the sibling cclsh directly. A missing, stale or slow
daemon therefore has the same result as launching cclsh normally.

The accelerated session uses a private pseudoterminal. It is not intended as a
login shell. Detached jobs are unsupported and may be terminated or lose their
terminal. Unusual sandbox, capability, signal, inherited-descriptor or terminal
contexts use ordinary cclsh. A nonempty GPG_TTY or SSH_TTY also selects the
direct runtime because it names the caller's real terminal. Ctrl-Z job control
inside CCLSH is relayed; externally suspending the proxy or worker itself is not
a supported accelerated-session operation.

The daemon must be started explicitly in the same trusted user security domain
and requires an absolute, owner-only XDG_RUNTIME_DIR. status succeeds only when
a matching worker is ready. stop and restart terminate active accelerated
sessions. Full source installation, fallback and process-context details:

  https://github.com/lambda-symbolics/cclsh/blob/master/docs/installation.org")

    ("install" "user and shared system installations"
     "The recommended Linux x86-64 package is the Nix flake:

  nix run github:lambda-symbolics/cclsh -- --version
  nix profile install github:lambda-symbolics/cclsh

From a checkout, use nix run . or nix profile install .#cclsh. The flake
rebuilds CCL 1.13 from the exact downstream CCL commits and editor dependency
revisions in dependencies.lock, and includes pinned Quicklisp metadata. nix
flake check builds and tests the installed result.

Every saved image uses an existing ~/quicklisp or initializes an embedded,
sanitized client and distribution index in a writable tree below
${XDG_DATA_HOME:-$HOME/.local/share}/cclsh/quicklisp. Override it with
CCLSH_QUICKLISP_HOME. Existing overrides must contain setup.lisp; invalid or
unwritable targets are reported and refused. Interactive and configured
sessions load local-init files, while plain commands, scripts and safe mode do
not. ASDF source-registry discovery remains lazy, so the first system lookup
may scan configured project trees but ordinary shell startup does not. The
flake does not edit /etc/shells or run chsh.

For SBCL source use on macOS or Linux, install Quicklisp and run
scripts/cclsh-sbcl from a checkout with its sibling dependencies; see
docs/installation.org. The SBCL runner has no saved image or cclsh-fast mode.

Source checks can use stock CCL. For a standalone saved image, select the exact
downstream CCL revision:

  git clone git@github.com:luciusmagn/ccl.git ../ccl
  git -C ../ccl checkout --detach 579c87300ee632af99182276f2ad40e1c38c5d0a
  make ccl-kernel CCL_SOURCE=../ccl

The fork is based on https://github.com/Clozure/ccl. Local files under
patches/ are byte-stable attestation mirrors. Nix fetches immutable diffs for
the exact fork commits. Select the rebuilt lx86cl64 and its matching boot image
for make build. After an attested build, install one system shell for every
account with:

  sudo make install-system-shell

The installer registers /usr/local/bin/cclsh in /etc/shells but never changes
an account. Each account then runs:

  chsh -s /usr/local/bin/cclsh

README.org has the complete source and system installation sequence.")

    ("scripting" "one-shots, scripts and shebangs"
     "Three non-interactive modes, all skipping startup.lisp and
history:

  cclsh -c 'echo one shot'        one command string
  cclsh provision.sh.lisp         a script file
  cclsh < input                   piped standard input

By convention, scripts are named NAME.sh.lisp. This canonical suffix is
not required: extensionless scripts and existing names still run. The
final .lisp keeps the file recognizable to Lisp tooling, while .sh marks
shell-oriented code. Command lines and shebangs can still make it
cclsh-specific rather than portable Common Lisp source that load can read
directly.

Load startup.lisp, but not history, for a configured one-shot:

  cclsh -lc 'z project'
  cclsh -ic 'echo $EDITOR'

Short flags combine in any order: -lc, -cl and -ilc are equivalent,
and -l -c works too. CCLSH_SAFE=1 suppresses startup.lisp even here.

The same built-in manual is available without starting a shell:

  cclsh help                       overview and section list
  cclsh help scripting             one section

Manual section arguments are passed directly to the help builtin. Use
cclsh -- help to run a script file literally named help.

Lines whose first non-whitespace character is ; are comments in every
mode. They may be indented and preserve the preceding exit status.

Script files work as shebang interpreters:

  #!/usr/local/bin/cclsh
  (format t \"deploying~%\")
  (all (make \"build\") (make \"deploy\"))

*argv* is a list containing the script path followed by every argument
after it, and is NIL outside script mode. Arguments beginning with a dash
remain script data. Use -- before a dash-prefixed script path:

  cclsh -- -provision.sh.lisp alpha

The saved CCL program keeps the executable basename cclsh so its patched
kernel recognizes and preserves command and script arguments. The SBCL source
launcher receives its arguments through its explicit -- boundary.

The process exit code is the last status, or the argument of exit. -c
is what ssh invokes, so remote commands skip user state. Programs already
visible in sshd's PATH work directly; user-profile-only tools need an
absolute path or a system-visible wrapper. The string uses cclsh syntax,
not sh.")

    ("login" "using cclsh as a login shell"
     "Install and register one root-owned shared copy outside the repository:

  scripts/check
  make system-shell-build CCL_SOURCE=../ccl CCL=../ccl/lx86cl64 \\
    CCL_IMAGE=/usr/lib/ccl/lx86cl64.image
  make integration-check CCL=../ccl/lx86cl64 \\
    CCL_IMAGE=/usr/lib/ccl/lx86cl64.image
  sudo make install-system-shell

This installs one user-neutral image at /usr/local/bin/cclsh and registers it
in /etc/shells. It never changes an account. Set PROBE_USER=USER to add a
clean-home launch as that account; the probe does not bind the installation to
that user. Reinstalling an attested release updates every account using the
stable path.

Each account keeps its own startup.lisp, history and writable Quicklisp state
under its home or XDG directories. Copy examples/startup.lisp to
~/.config/cclsh/startup.lisp, keep the directory mode 0700 and the file mode
0600, then verify and switch that account:

  /usr/local/bin/cclsh --version
  env CCLSH_SAFE=1 /usr/local/bin/cclsh -c 'exit 0'
  /usr/local/bin/cclsh -lc 'echo $PATH'
  chsh -s /usr/local/bin/cclsh

Plain -c skips all user state for remote safety. -lc, -cl, -ic and
-l -c load startup.lisp before running their command, so $SHELL -lc
sees the configured login environment. Other flags are ignored so odd
login invocations cannot lock you out. system-shell-build attests the required
CCL patches and exact kernel/image hashes. install-system-shell rejects an
absent or stale attestation, validates the candidate before activation, then
registers its stable path in /etc/shells. Registration failure restores the
previous release. An administrator can change another account with:

  sudo chsh -s /usr/local/bin/cclsh USER

Probe optional startup tools such as zoxide separately after the guaranteed
version, safe command and configured PATH checks pass.

Nothing sources /etc/profile for you. Keep /usr/local/bin, /usr/bin and /bin
in PATH.

The conservative choice is to keep root or another privileged recovery
account on a stock shell. If root uses cclsh, keep an existing privileged
session open while testing, verify boot-loader or live-system recovery, and use
the same shared path:

  sudo chsh -s /usr/local/bin/cclsh root

Emergency access past broken user state:

  env CCLSH_SAFE=1 /usr/local/bin/cclsh
  sudo chsh -s /usr/bin/fish USER"))
  "The built-in manual: (name one-liner body) per section.")

(defun manual--heading (text)
  "Render a manual heading, colored only on a terminal."
  (terminal-colorize text ':cyan :bold t))

(defun manual--print-overview ()
  "Print the manual overview and section list."
  (format t "~a~%~%" (manual--heading "cclsh"))
  (format t "A system shell inside a live Common Lisp session. One rule: a line~%~
             starting with ( is Lisp, anything else is a command. The sections~%~
             below cover what is special; the rest is ordinary Common Lisp.~%~%")
  (dolist (section *manual-sections*)
    (destructuring-bind (name one-liner body) section
      (declare (ignore body))
      (format t "  ~a~a~%"
              (terminal-colorize (format nil "~14a" name) ':cyan)
              one-liner)))
  (format t "~%help TOPIC or cclsh help TOPIC prints a section or command.~%~
             commands lists what is callable, --version identifies the build,~%~
             and docs/guide.org is the long form.~%"))

(defun manual--print-section (name)
  "Print one manual section. Returns true when NAME exists."
  (let ((section (assoc name *manual-sections* :test #'string-equal)))
    (if (null section)
        (progn
          (format *error-output* "~a~%"
                  (terminal-colorize
                   (format nil "help: no section or command ~a; manual sections are ~{~a~^, ~}"
                           name
                           (mapcar #'first *manual-sections*))
                   ':red))
          nil)
        (destructuring-bind (section-name one-liner body) section
          (format t "~a  ~a~%~%~a~%"
                  (manual--heading section-name)
                  one-liner
                  body)
          t))))

(defcommand help (&rest sections)
  "Show the built-in manual or generated command help."
  (if (null sections)
      (progn
        (manual--print-overview)
        0)
      (let ((status 0))
        (dolist (section sections status)
          (let ((name (string-downcase (princ-to-string section))))
            (cond ((assoc name *manual-sections* :test #'string-equal)
                   (manual--print-section name))
                  (t
                   (multiple-value-bind (kind command)
                       (command-resolve name)
                     (if (eq kind ':builtin)
                         (command-print-help command)
                         (progn
                           (manual--print-section name)
                           (setf status 1)))))))))))
