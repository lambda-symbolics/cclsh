#!/bin/sh
# Exercise the real saved kernel and image through their installed symlink.
set -eu
cd "$(dirname "$0")/.."
. scripts/platform

if [ ! -x ./cclsh ] || [ ! -s ./cclsh.image ]; then
    echo "installed-image check requires a completed scripts/build" >&2
    exit 2
fi
if ! command -v bash >/dev/null 2>&1; then
    echo "installed-image check requires bash for login-style argv[0]" >&2
    exit 2
fi

temporary_directory=$(mktemp -d "${TMPDIR:-/tmp}/cclsh-image-check.XXXXXX")
cleanup()
{
    rm -rf -- "$temporary_directory"
}
trap cleanup 0
trap 'exit 129' 1
trap 'exit 130' 2
trap 'exit 143' 15

install_directory=$temporary_directory/bin
CCLSH_SKIP_BUILD=1 \
CCLSH_KERNEL_ARTIFACT="$PWD/cclsh" \
CCLSH_IMAGE_ARTIFACT="$PWD/cclsh.image" \
CCLSH_INSTALL_DIRECTORY="$install_directory" \
scripts/install >/dev/null

shell_path=$install_directory/cclsh
resolved_shell=$(cclsh_realpath_existing "$shell_path")
resolved_stable_image=$(cclsh_realpath_existing "$shell_path.image")
if [ ! -L "$shell_path" ] || [ ! -L "$shell_path.image" ] ||
   [ ! -f "$resolved_shell.image" ] ||
   [ "$resolved_stable_image" != "$resolved_shell.image" ]
then
    echo "real install did not activate a matched kernel and image" >&2
    exit 1
fi
if ! cmp -s "$PWD/cclsh" "$resolved_shell" ||
   ! cmp -s "$PWD/cclsh.image" "$resolved_shell.image" ||
   [ "$(cclsh_stat_mode "$resolved_shell")" != 755 ] ||
   [ "$(cclsh_stat_mode "$resolved_shell.image")" != 600 ]
then
    echo "installed release content or modes differ from the build" >&2
    exit 1
fi

validation_home=$temporary_directory/home
mkdir -m 700 "$validation_home"
env -i \
    HOME="$validation_home" \
    XDG_CONFIG_HOME="$validation_home/.config" \
    PATH=/usr/local/bin:/usr/bin:/bin \
    SHELL="$shell_path" \
    CCLSH_SAFE=1 \
    LANG=C \
    LC_ALL=C \
    "$shell_path" --version >/dev/null
marker=$(
    env -i \
        HOME="$validation_home" \
        XDG_CONFIG_HOME="$validation_home/.config" \
        PATH=/usr/local/bin:/usr/bin:/bin \
        SHELL="$shell_path" \
        CCLSH_SAFE=1 \
        LANG=C \
        LC_ALL=C \
        "$shell_path" -c 'echo __INSTALLED_IMAGE__'
)
if [ "$marker" != __INSTALLED_IMAGE__ ]; then
    echo "installed image command probe returned unexpected output: $marker" >&2
    exit 1
fi

login_shell=$(
    env -i \
        HOME="$validation_home" \
        XDG_CONFIG_HOME="$validation_home/.config" \
        PATH=/usr/local/bin:/usr/bin:/bin \
        SHELL="$shell_path" \
        CCLSH_SAFE=1 \
        LANG=C \
        LC_ALL=C \
        bash -c 'exec -a -cclsh "$1" -c "$2"' \
        bash "$shell_path" 'echo $SHELL'
)
if [ "$login_shell" != "$shell_path" ]; then
    echo "login-style argv[0] lost the stable SHELL path: $login_shell" >&2
    exit 1
fi

script_path=$temporary_directory/installed-argv.sh.lisp
printf '%s\n' \
    '(progn (format t "__INSTALLED_ARGV__~s__~%" *argv*) (values))' \
    >"$script_path"
script_output=$(
    env -i \
        HOME="$validation_home" \
        XDG_CONFIG_HOME="$validation_home/.config" \
        PATH=/usr/local/bin:/usr/bin:/bin \
        SHELL="$shell_path" \
        CCLSH_SAFE=1 \
        LANG=C \
        LC_ALL=C \
        "$shell_path" "$script_path" --no-avx --stack-size 42 \
            -Iignored-image ""
)
expected_script_output=$(printf \
    '__INSTALLED_ARGV__("%s" "--no-avx" "--stack-size" "42" %s)__' \
    "$script_path" '"-Iignored-image" ""')
if [ "$script_output" != "$expected_script_output" ]; then
    echo "installed image lost script arguments: $script_output" >&2
    exit 1
fi

echo "Installed saved-image checks passed."
