#!/bin/sh
# Exercise operating-system adapters used by build and installation scripts.
set -eu
cd "$(dirname "$0")/.."
. scripts/platform

temporary_directory=$(
    mktemp -d "${TMPDIR:-/tmp}/cclsh-platform-check.XXXXXX"
)
cleanup()
{
    rm -rf -- "$temporary_directory"
}
trap cleanup 0
trap 'exit 129' 1
trap 'exit 130' 2
trap 'exit 143' 15

mkdir -p "$temporary_directory/existing/child"
expected_existing=$(CDPATH= cd -- "$temporary_directory/existing" && pwd -P)
if [ "$(cclsh_realpath_existing "$temporary_directory/existing/child/..")" != \
     "$expected_existing" ]
then
    echo "platform adapter did not resolve an existing path" >&2
    exit 1
fi
if cclsh_realpath_existing "$temporary_directory/missing" >/dev/null 2>&1; then
    echo "platform adapter accepted a missing existing path" >&2
    exit 1
fi
ln -s missing "$temporary_directory/dangling"
if cclsh_realpath_existing "$temporary_directory/dangling" >/dev/null 2>&1
then
    echo "platform adapter accepted a dangling existing path" >&2
    exit 1
fi
expected_missing=$temporary_directory/existing/target
if [ "$(cclsh_realpath_missing \
          "$temporary_directory/existing/missing/../target")" != \
     "$expected_missing" ]
then
    echo "platform adapter did not normalize a missing path" >&2
    exit 1
fi

source_file=$temporary_directory/source
copy_file=$temporary_directory/copy
printf '%s\n' portability >"$source_file"
chmod 640 "$source_file"
cclsh_copy_preserving "$source_file" "$copy_file"
if ! cmp -s "$source_file" "$copy_file" ||
   [ "$(cclsh_stat_mode "$copy_file")" != 640 ] ||
   [ "$(cclsh_stat_owner "$copy_file")" != "$(id -u)" ] ||
   [ "$(cclsh_stat_group "$copy_file")" != "$(id -g)" ] ||
   cclsh_path_is_group_or_world_writable "$copy_file"
then
    echo "platform adapter did not preserve safe file metadata" >&2
    exit 1
fi
chmod 660 "$copy_file"
if ! cclsh_path_is_group_or_world_writable "$copy_file"; then
    echo "platform adapter did not detect a writable file" >&2
    exit 1
fi

replacement=$temporary_directory/replacement
destination=$temporary_directory/destination
printf '%s\n' old >"$destination"
printf '%s\n' new >"$replacement"
cclsh_atomic_replace "$replacement" "$destination"
if [ "$(cat "$destination")" != new ] || [ -e "$replacement" ]; then
    echo "platform adapter did not atomically replace a file" >&2
    exit 1
fi

replacement=$temporary_directory/symlink-replacement
destination=$temporary_directory/symlink-destination
destination_directory=$temporary_directory/symlink-target
mkdir "$destination_directory"
ln -s "$destination_directory" "$destination"
printf '%s\n' replaced >"$replacement"
cclsh_atomic_replace "$replacement" "$destination"
if [ -L "$destination" ] ||
   [ "$(cat "$destination")" != replaced ] ||
   [ -e "$destination_directory/$(basename "$replacement")" ]
then
    echo "platform adapter followed a destination directory symlink" >&2
    exit 1
fi

staging=$temporary_directory/staging
release=$temporary_directory/release
mkdir "$staging"
printf '%s\n' release >"$staging/artifact"
cclsh_publish_directory "$staging" "$release"
if [ "$(cat "$release/artifact")" != release ] || [ -e "$staging" ]; then
    echo "platform adapter did not publish a directory" >&2
    exit 1
fi

digest=$(printf test | scripts/stream-sha256)
digest=${digest%% *}
if [ "$digest" != \
     9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08 ]
then
    echo "platform adapter returned the wrong SHA-256 digest" >&2
    exit 1
fi

lock_project=$temporary_directory/lock-project
mkdir -p "$lock_project/scripts"
cp scripts/platform scripts/with-build-lock scripts/build-lock-held \
    "$lock_project/scripts/"
if ! (
    cd "$lock_project"
    scripts/with-build-lock sh -c 'scripts/build-lock-held'
); then
    echo "platform adapter did not preserve the build lock" >&2
    exit 1
fi

if [ "$(id -u)" -eq 0 ] && id nobody >/dev/null 2>&1; then
    expected_uid=$(id -u nobody)
    actual_uid=$(scripts/run-as-user nobody /usr/bin/id -u)
    if [ "$actual_uid" != "$expected_uid" ]; then
        echo "platform adapter did not switch probe identity" >&2
        exit 1
    fi
fi

case "$cclsh_system_name" in
    Linux)
        [ "$cclsh_default_ccl_kernel" = lx86cl64 ] || exit 1
        [ "$cclsh_default_ccl_kernel_directory" = linuxx8664 ] || exit 1
        ;;
    NetBSD)
        [ "$cclsh_default_ccl_kernel" = narmcl ] || exit 1
        [ "$cclsh_default_ccl_kernel_directory" = netbsdarm ] || exit 1
        ;;
    *)
        echo "platform adapter test does not support $cclsh_system_name" >&2
        exit 2
        ;;
esac

echo "Platform tooling checks passed."
