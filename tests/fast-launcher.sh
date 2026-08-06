#!/bin/sh
# Exercise cclsh-fast fallback, daemon lifecycle and interactive PTY relay.
set -eu
cd "$(dirname "$0")/.."

if [ "$(uname -s)" != Linux ]; then
    echo "Fast-launcher checks skipped: Linux required."
    exit 0
fi

if [ ! -x ./cclsh ] || [ ! -s ./cclsh.image ]; then
    echo "fast-launcher check requires a completed scripts/build" >&2
    exit 2
fi
for program in cmp grep mktemp realpath script sleep stty timeout; do
    if ! command -v "$program" >/dev/null 2>&1; then
        echo "fast-launcher check requires $program" >&2
        exit 2
    fi
done

scripts/build-fast
if [ ! -x ./cclsh-fast ]; then
    echo "scripts/build-fast did not create cclsh-fast" >&2
    exit 1
fi

temporary_directory=$(
    mktemp -d "${TMPDIR:-/tmp}/cclsh-fast-launcher-check.XXXXXX"
)
home=$temporary_directory/home
runtime=$temporary_directory/runtime
config=$temporary_directory/config
mkdir -m 700 "$home" "$runtime" "$config"

insecure_runtime=$temporary_directory/insecure-runtime
mkdir -m 755 "$insecure_runtime"

direct_shell=$(realpath -e ./cclsh)
fast_shell=$(realpath -e ./cclsh-fast)

fast_environment()
{
    env -i \
        HOME="$home" \
        XDG_CONFIG_HOME="$config" \
        XDG_RUNTIME_DIR="$runtime" \
        PATH=/usr/local/bin:/usr/bin:/bin \
        SHELL="$fast_shell" \
        CCLSH_SAFE=1 \
        LANG=C \
        LC_ALL=C \
        "$@"
}

daemon_command()
{
    fast_environment "$fast_shell" daemon "$@"
}

cleanup()
{
    daemon_command stop >/dev/null 2>&1 || true
    rm -rf -- "$temporary_directory"
}
trap cleanup 0
trap 'exit 129' 1
trap 'exit 130' 2
trap 'exit 143' 15

fail()
{
    echo "fast-launcher check: $*" >&2
    exit 1
}

capture()
{
    capture_prefix=$1
    shift
    set +e
    fast_environment "$@" \
        >"$capture_prefix.stdout" 2>"$capture_prefix.stderr"
    capture_status=$?
    set -e
    printf '%s\n' "$capture_status" >"$capture_prefix.status"
}

require_same_capture()
{
    capture_label=$1
    capture_left=$2
    capture_right=$3
    for capture_suffix in stdout stderr status; do
        if ! cmp -s "$capture_left.$capture_suffix" \
                  "$capture_right.$capture_suffix"
        then
            fail "$capture_label differs in $capture_suffix"
        fi
    done
}

wait_for_daemon()
{
    wait_attempt=0
    while [ "$wait_attempt" -lt 100 ]; do
        if wait_status=$(daemon_command status 2>/dev/null); then
            if printf '%s\n' "$wait_status" |
               grep -Eq '(^|[[:space:]])ready=1([[:space:]]|$)'
            then
                return 0
            fi
        fi
        wait_attempt=$((wait_attempt + 1))
        sleep 0.02
    done
    return 1
}

wait_for_no_daemon()
{
    wait_attempt=0
    while [ "$wait_attempt" -lt 100 ]; do
        if ! daemon_command status >/dev/null 2>&1; then
            return 0
        fi
        wait_attempt=$((wait_attempt + 1))
        sleep 0.02
    done
    return 1
}


# An absent daemon must make every noninteractive form an exact direct exec.
daemon_command stop >/dev/null 2>&1 || true
wait_for_no_daemon || fail "daemon remained live before fallback checks"

capture "$temporary_directory/direct-version" "$direct_shell" --version
capture "$temporary_directory/fast-version" "$fast_shell" --version
require_same_capture "absent-daemon --version fallback" \
    "$temporary_directory/direct-version" \
    "$temporary_directory/fast-version"

capture "$temporary_directory/worker-variable-version" \
    env CCLSH_FAST_WORKER_FD=3 "$direct_shell" --version
require_same_capture "private worker variable without marker" \
    "$temporary_directory/direct-version" \
    "$temporary_directory/worker-variable-version"

worker_variable_form='(progn (format t "__FAST_WORKER_VARIABLE__~a__~%" (getenv "CCLSH_FAST_WORKER_FD")) (exit 0))'
capture "$temporary_directory/direct-worker-variable" \
    env CCLSH_FAST_WORKER_FD=external "$direct_shell" -c \
        "$worker_variable_form"
capture "$temporary_directory/fast-worker-variable" \
    env CCLSH_FAST_WORKER_FD=external "$fast_shell" -c \
        "$worker_variable_form"
require_same_capture "private worker variable direct fallback" \
    "$temporary_directory/direct-worker-variable" \
    "$temporary_directory/fast-worker-variable"

command_form='(progn (format t "__FAST_FALLBACK_COMMAND__~%") (exit 37))'
capture "$temporary_directory/direct-command" \
    "$direct_shell" -c "$command_form"
capture "$temporary_directory/fast-command" \
    "$fast_shell" -c "$command_form"
require_same_capture "absent-daemon -c fallback" \
    "$temporary_directory/direct-command" \
    "$temporary_directory/fast-command"

script_path=$temporary_directory/arguments.sh.lisp
printf '%s\n' \
    '(progn (format t "__FAST_SCRIPT_ARGV__~s__~%" *argv*) (exit 23))' \
    >"$script_path"
capture "$temporary_directory/direct-script" \
    "$direct_shell" "$script_path" --no-avx -R report -S serve ""
capture "$temporary_directory/fast-script" \
    "$fast_shell" "$script_path" --no-avx -R report -S serve ""
require_same_capture "absent-daemon script fallback" \
    "$temporary_directory/direct-script" \
    "$temporary_directory/fast-script"

plain_input=$temporary_directory/plain-input
printf '%s\n' \
    '(progn (format t "__FAST_PLAIN_INPUT__~%") (exit 19))' \
    >"$plain_input"
set +e
fast_environment "$direct_shell" \
    <"$plain_input" \
    >"$temporary_directory/direct-plain.stdout" \
    2>"$temporary_directory/direct-plain.stderr"
direct_plain_status=$?
fast_environment "$fast_shell" \
    <"$plain_input" \
    >"$temporary_directory/fast-plain.stdout" \
    2>"$temporary_directory/fast-plain.stderr"
fast_plain_status=$?
set -e
printf '%s\n' "$direct_plain_status" \
    >"$temporary_directory/direct-plain.status"
printf '%s\n' "$fast_plain_status" \
    >"$temporary_directory/fast-plain.status"
require_same_capture "absent-daemon non-TTY fallback" \
    "$temporary_directory/direct-plain" \
    "$temporary_directory/fast-plain"

if env -i \
       HOME="$home" \
       XDG_CONFIG_HOME="$config" \
       XDG_RUNTIME_DIR="$insecure_runtime" \
       PATH=/usr/local/bin:/usr/bin:/bin \
       LANG=C \
       LC_ALL=C \
       "$fast_shell" daemon start >/dev/null 2>&1
then
    fail "daemon accepted an insecure runtime directory"
fi


# Exercise the public daemon lifecycle before leasing an interactive worker.
daemon_command start >/dev/null
wait_for_daemon || fail "daemon did not become ready after start"

# Noninteractive interfaces must remain exact direct execs with a ready daemon.
capture "$temporary_directory/ready-fast-version" "$fast_shell" --version
require_same_capture "ready-daemon --version fallback" \
    "$temporary_directory/direct-version" \
    "$temporary_directory/ready-fast-version"
capture "$temporary_directory/ready-fast-command" \
    "$fast_shell" -c "$command_form"
require_same_capture "ready-daemon -c fallback" \
    "$temporary_directory/direct-command" \
    "$temporary_directory/ready-fast-command"
capture "$temporary_directory/ready-fast-script" \
    "$fast_shell" "$script_path" --no-avx -R report -S serve ""
require_same_capture "ready-daemon script fallback" \
    "$temporary_directory/direct-script" \
    "$temporary_directory/ready-fast-script"
set +e
fast_environment "$fast_shell" \
    <"$plain_input" \
    >"$temporary_directory/ready-fast-plain.stdout" \
    2>"$temporary_directory/ready-fast-plain.stderr"
ready_fast_plain_status=$?
set -e
printf '%s\n' "$ready_fast_plain_status" \
    >"$temporary_directory/ready-fast-plain.status"
require_same_capture "ready-daemon non-TTY fallback" \
    "$temporary_directory/direct-plain" \
    "$temporary_directory/ready-fast-plain"
ready_fallback_status=$(daemon_command status)
printf '%s\n' "$ready_fallback_status" |
    grep -Eq '(^|[[:space:]])active=0([[:space:]]|$)' ||
    fail "noninteractive fallback leased a worker"

# Rebuilding only the native launcher must retire its stale supervisor.
scripts/build-fast
daemon_command start >/dev/null
wait_for_daemon || fail "rebuilt launcher did not replace stale daemon"
daemon_command restart >/dev/null
wait_for_daemon || fail "daemon did not become ready after restart"

session_directory=$temporary_directory/session-directory
mkdir "$session_directory"
context_probe=$temporary_directory/context-probe
printf '%s\n' \
    '#!/bin/sh' \
    'printf '\''__FAST_CWD__%s__\n'\'' "$(pwd -P)"' \
    'printf '\''__FAST_ENV__%s__\n'\'' "$CCLSH_FAST_TEST_VALUE"' \
    'printf '\''__FAST_UMASK__%s__\n'\'' "$(umask)"' \
    >"$context_probe"
chmod 755 "$context_probe"

session_driver=$temporary_directory/session-driver
printf '%s\n' \
    '#!/bin/sh' \
    'before=$(stty -g) || exit 1' \
    '"$CCLSH_FAST_PATH"' \
    'shell_status=$?' \
    'after=$(stty -g) || exit 1' \
    'if [ "$before" = "$after" ]; then' \
    '    printf '\''__FAST_TTY_RESTORED__\n'\''' \
    'else' \
    '    printf '\''__FAST_TTY_DAMAGED__%s:%s__\n'\'' "$before" "$after"' \
    '    exit 1' \
    'fi' \
    'exit "$shell_status"' \
    >"$session_driver"
chmod 755 "$session_driver"

interactive_output=$temporary_directory/interactive.stdout
interactive_error=$temporary_directory/interactive.stderr
typescript=$temporary_directory/interactive.typescript
set +e
(
    printf '(run "%s" "daemon" "status")\n' "$fast_shell"
    printf '(run "%s")\n' "$context_probe"
    printf '(format t "__FAST_ARGV_~a__~%%" '
    printf '(and (= (length ccl:*command-line-argument-list*) 1) '
    printf '(null ccl:*unprocessed-command-line-arguments*)))\n'
    printf '(run "/usr/bin/sleep" "10")\n'
    sleep 0.5
    printf '\003'
    sleep 0.2
    printf '(format t "__FAST_AFTER_~a__~%%" "INTERRUPT")\n'
    printf '(run "/usr/bin/sleep" "10")\n'
    sleep 0.5
    printf '\032'
    sleep 0.2
    printf 'fg\n'
    sleep 0.5
    printf '\003'
    sleep 0.2
    printf '(format t "__FAST_AFTER_~a__~%%" "SUSPEND")\n'
    printf '(run "/usr/bin/seq" "1" "20000")\n'
    printf '(format t "__FAST_AFTER_~a__~%%" "LARGE_OUTPUT")\n'
    printf 'exit 0\n'
) | (
    cd "$session_directory"
    umask 027
    timeout -k 2 12 \
        env -i \
            HOME="$home" \
            XDG_CONFIG_HOME="$config" \
            XDG_RUNTIME_DIR="$runtime" \
            PATH=/usr/local/bin:/usr/bin:/bin \
            SHELL="$fast_shell" \
            CCLSH_SAFE=1 \
            CCLSH_FAST_PATH="$fast_shell" \
            CCLSH_FAST_TEST_VALUE='context-value' \
            LANG=C \
            LC_ALL=C \
            script -qefc "$session_driver" "$typescript"
) >"$interactive_output" 2>"$interactive_error"
interactive_status=$?
set -e
if [ "$interactive_status" -ne 0 ]; then
    sed -n '1,160p' "$interactive_output" >&2
    sed -n '1,160p' "$interactive_error" >&2
    fail "interactive accelerated session exited $interactive_status"
fi

grep -Eq '(^|[[:space:]])active=1([[:space:]]|$)' \
    "$interactive_output" || fail "interactive shell did not lease a worker"
grep -Fq "__FAST_CWD__$(realpath -e "$session_directory")__" \
    "$interactive_output" || fail "worker did not receive the client cwd"
grep -Fq '__FAST_ENV__context-value__' "$interactive_output" ||
    fail "worker did not receive the client environment"
grep -Fq '__FAST_UMASK__0027__' "$interactive_output" ||
    fail "worker did not receive the client umask"
grep -Fq '__FAST_ARGV_T__' "$interactive_output" ||
    fail "worker exposed its private command-line marker"
grep -Fq '__FAST_AFTER_INTERRUPT__' "$interactive_output" ||
    fail "Ctrl-C did not return control to the accelerated shell"
grep -Fq '__FAST_AFTER_SUSPEND__' "$interactive_output" ||
    fail "Ctrl-Z did not return control to the accelerated shell"
grep -Fq '__FAST_AFTER_LARGE_OUTPUT__' "$interactive_output" ||
    fail "large relayed output stalled or lost its trailing marker"
grep -Fq '__FAST_TTY_RESTORED__' "$interactive_output" ||
    fail "client did not restore its terminal attributes"
if grep -Fq '__FAST_TTY_DAMAGED__' "$interactive_output"; then
    fail "accelerated session damaged terminal attributes"
fi

wait_for_daemon || fail "daemon did not replenish its leased worker"
post_session_status=$(daemon_command status)
printf '%s\n' "$post_session_status" |
    grep -Eq '(^|[[:space:]])active=0([[:space:]]|$)' ||
    fail "daemon retained an active worker after shell exit"

# A leased worker must load the caller's ordinary startup file after context
# transfer, not the daemon's startup state.
mkdir -p "$config/cclsh"
printf '%s\n' \
    '(format t "__FAST_STARTUP_~a__~%" "LOADED")' \
    >"$config/cclsh/startup.lisp"
startup_output=$temporary_directory/startup.stdout
(
    printf '(run "%s" "daemon" "status")\n' "$fast_shell"
    printf 'exit 0\n'
) | timeout -k 2 4 \
    env -i \
        HOME="$home" \
        XDG_CONFIG_HOME="$config" \
        XDG_RUNTIME_DIR="$runtime" \
        PATH=/usr/local/bin:/usr/bin:/bin \
        SHELL="$fast_shell" \
        LANG=C \
        LC_ALL=C \
        script -qefc "$fast_shell" /dev/null \
    >"$startup_output" 2>&1
grep -Fq '__FAST_STARTUP_LOADED__' "$startup_output" ||
    fail "accelerated shell did not load the caller startup file"
grep -Eq '(^|[[:space:]])active=1([[:space:]]|$)' "$startup_output" ||
    fail "startup-file check did not lease a worker"
wait_for_daemon || fail "daemon did not replenish after startup-file check"

# A committed shell's nonzero status must cross the broker and proxy intact.
exit_driver=$temporary_directory/exit-driver
printf '%s\n' \
    '#!/bin/sh' \
    'before=$(stty -g) || exit 1' \
    '"$CCLSH_FAST_PATH"' \
    'shell_status=$?' \
    'after=$(stty -g) || exit 1' \
    'test "$before" = "$after" || exit 1' \
    'exit "$shell_status"' \
    >"$exit_driver"
chmod 755 "$exit_driver"
set +e
(
    printf '(run "%s" "daemon" "status")\n' "$fast_shell"
    printf 'exit 23\n'
) | fast_environment \
    timeout -k 2 4 \
    env CCLSH_FAST_PATH="$fast_shell" \
    script -qefc "$exit_driver" /dev/null \
    >"$temporary_directory/exit-status.stdout" 2>&1
accelerated_exit_status=$?
set -e
if [ "$accelerated_exit_status" -ne 23 ]; then
    fail "accelerated shell returned $accelerated_exit_status instead of 23"
fi
grep -Eq '(^|[[:space:]])active=1([[:space:]]|$)' \
    "$temporary_directory/exit-status.stdout" ||
    fail "exit-status check did not lease a worker"
wait_for_daemon || fail "daemon did not replenish after exit-status check"

# Exec-sensitive environment differences must choose the direct runtime.
context_fallback_output=$temporary_directory/context-fallback.stdout
(
    printf '(run "%s" "daemon" "status")\n' "$fast_shell"
    printf 'exit 0\n'
) | timeout -k 2 4 \
    env -i \
        HOME="$home" \
        XDG_CONFIG_HOME="$config" \
        XDG_RUNTIME_DIR="$runtime" \
        PATH=/usr/local/bin:/usr/bin:/bin \
        SHELL=/bin/sh \
        CCLSH_SAFE=1 \
        LANG=C.UTF-8 \
        LC_ALL=C \
        script -qefc "$fast_shell" /dev/null \
    >"$context_fallback_output" 2>&1
grep -Eq '(^|[[:space:]])active=0([[:space:]]|$)' \
    "$context_fallback_output" ||
    fail "exec-sensitive environment mismatch did not fall back"
if grep -Eq '(^|[[:space:]])active=1([[:space:]]|$)' \
        "$context_fallback_output"
then
    fail "environment-mismatched session leased a worker"
fi

# Terminal-path variables bypass the proxy, so they must force direct exec.
tty_environment_output=$temporary_directory/tty-environment-fallback.stdout
(
    printf '(run "%s" "daemon" "status")\n' "$fast_shell"
    printf 'exit 0\n'
) | timeout -k 2 4 \
    env -i \
        HOME="$home" \
        XDG_CONFIG_HOME="$config" \
        XDG_RUNTIME_DIR="$runtime" \
        PATH=/usr/local/bin:/usr/bin:/bin \
        SHELL=/bin/sh \
        CCLSH_SAFE=1 \
        GPG_TTY=/dev/tty \
        LANG=C \
        LC_ALL=C \
        script -qefc "$fast_shell" /dev/null \
    >"$tty_environment_output" 2>&1
grep -Eq '(^|[[:space:]])active=0([[:space:]]|$)' \
    "$tty_environment_output" ||
    fail "terminal-path environment did not force direct fallback"

# A narrower CPU affinity than the daemon's must force direct exec.
if command -v taskset >/dev/null 2>&1 &&
   command -v nproc >/dev/null 2>&1 && [ "$(nproc)" -gt 1 ]
then
    allowed_cpus=$(awk '/^Cpus_allowed_list:/ { print $2 }' /proc/self/status)
    first_cpu=${allowed_cpus%%,*}
    first_cpu=${first_cpu%%-*}
    affinity_output=$temporary_directory/affinity-fallback.stdout
    (
        printf '(run "%s" "daemon" "status")\n' "$fast_shell"
        printf 'exit 0\n'
    ) | timeout -k 2 4 \
        env -i \
            HOME="$home" \
            XDG_CONFIG_HOME="$config" \
            XDG_RUNTIME_DIR="$runtime" \
            PATH=/usr/local/bin:/usr/bin:/bin \
            SHELL=/bin/sh \
            CCLSH_SAFE=1 \
            LANG=C \
            LC_ALL=C \
            script -qefc "taskset -c $first_cpu $fast_shell" /dev/null \
        >"$affinity_output" 2>&1
    grep -Eq '(^|[[:space:]])active=0([[:space:]]|$)' \
        "$affinity_output" ||
        fail "CPU-affinity mismatch did not force direct fallback"
fi

# An intentional inheritable descriptor must choose the direct runtime.
descriptor_fallback_output=$temporary_directory/descriptor-fallback.stdout
descriptor_file=$temporary_directory/inherited-descriptor
descriptor_command="exec 9<>\"$descriptor_file\"; exec \"$fast_shell\""
(
    printf '(run "%s" "daemon" "status")\n' "$fast_shell"
    printf 'exit 0\n'
) | timeout -k 2 4 \
    env -i \
        HOME="$home" \
        XDG_CONFIG_HOME="$config" \
        XDG_RUNTIME_DIR="$runtime" \
        PATH=/usr/local/bin:/usr/bin:/bin \
        SHELL=/bin/sh \
        CCLSH_SAFE=1 \
        LANG=C \
        LC_ALL=C \
        script -qefc "/bin/sh -c '$descriptor_command'" /dev/null \
    >"$descriptor_fallback_output" 2>&1
grep -Eq '(^|[[:space:]])active=0([[:space:]]|$)' \
    "$descriptor_fallback_output" ||
    fail "inheritable descriptor session did not fall back"

# Losing the daemon after commit must fail once, restore the terminal and never
# start a second direct shell on the remaining input.
post_commit_output=$temporary_directory/post-commit.stdout
(
    sleep 0.4
    daemon_command stop >/dev/null 2>&1 || true
) &
stopper=$!
set +e
(
    printf '(run "%s" "daemon" "status")\n' "$fast_shell"
    printf '(run "/usr/bin/sleep" "10")\n'
    printf '(format t "__FAST_SECOND_~a__~%%" "SHELL")\n'
    printf 'exit 0\n'
) | fast_environment \
    timeout -k 2 4 \
    env CCLSH_FAST_PATH="$fast_shell" \
    script -qefc "$session_driver" /dev/null \
    >"$post_commit_output" 2>&1
post_commit_status=$?
set -e
wait "$stopper" || true
if [ "$post_commit_status" -ne 70 ]; then
    fail "post-commit daemon loss returned $post_commit_status instead of 70"
fi
grep -Eq '(^|[[:space:]])active=1([[:space:]]|$)' "$post_commit_output" ||
    fail "post-commit failure check did not lease a worker"
grep -Fq '__FAST_TTY_RESTORED__' "$post_commit_output" ||
    fail "post-commit daemon loss did not restore terminal attributes"
if grep -Fq '__FAST_SECOND_SHELL__' "$post_commit_output"; then
    fail "post-commit daemon loss started or continued a second shell"
fi

wait_for_no_daemon || fail "daemon remained live after stop"

echo "Fast launcher checks passed."
