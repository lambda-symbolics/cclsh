#!/bin/sh
# Regression checks for one shared, root-owned system shell installation.
set -eu
cd "$(dirname "$0")/.."
. scripts/platform
run_as_user=$PWD/scripts/run-as-user

if [ "$(id -u)" -ne 0 ]; then
    echo "Shared system-shell installation checks skipped: root required."
    exit 0
fi
if ! id nobody >/dev/null 2>&1; then
    echo "Shared system-shell installation checks skipped: nobody missing."
    exit 0
fi

if ! scratch_parent=$(cclsh_root_scratch_parent); then
    echo "system install check: no safe root scratch directory" >&2
    exit 1
fi
temporary_directory=$(
    mktemp -d "$scratch_parent/cclsh-system-install-check.XXXXXX"
)
cleanup()
{
    rm -rf -- "$temporary_directory"
}
trap cleanup 0
trap 'exit 129' 1
trap 'exit 130' 2
trap 'exit 143' 15
chmod 755 "$temporary_directory"

source_shell=$temporary_directory/source-cclsh
source_image=$temporary_directory/source-cclsh.image
attestation=$temporary_directory/cclsh.attestation
install_directory=$temporary_directory/system-bin
shell_path=$install_directory/cclsh
shells_file=$temporary_directory/shells
nobody_uid=$(id -u nobody)
nobody_gid=$(id -g nobody)

write_source_shell()
{
    source_version=$1
    denied_uid=$2
    cat >"$source_shell" <<EOF
#!/bin/sh
if test "$denied_uid" != none && test "\$(id -u)" = "$denied_uid"; then
    exit 71
fi
case "\${1:-}" in
    --version)
        printf '%s\n' 'cclsh system test $source_version'
        exit 0
        ;;
    -c)
        exit 0
        ;;
    *)
        exit 2
        ;;
esac
EOF
    chmod 755 "$source_shell"
}

write_test_attestation()
{
    attestation_path=$1
    attested_kernel=$2
    attested_image=$3
    kernel_hash=$(scripts/file-sha256 "$attested_kernel")
    image_hash=$(scripts/file-sha256 "$attested_image")
    {
        printf '%s\n' 'cclsh-login-build-v1'
        printf 'kernel-sha256 %s\n' "$kernel_hash"
        printf 'image-sha256 %s\n' "$image_hash"
        for patch in $cclsh_required_ccl_patches; do
            patch_hash=$(scripts/file-sha256 "$patch")
            printf 'patch-sha256 %s %s\n' \
                "$(basename "$patch")" \
                "$patch_hash"
        done
    } >"$attestation_path"
    chmod 600 "$attestation_path"
}

assert_system_release()
{
    checked_release=$1
    checked_directory=$(dirname "$checked_release")
    if [ "$(test_path_metadata "$checked_directory")" != 755:0:0 ] ||
       [ "$(test_path_metadata "$checked_release")" != 755:0:0 ] ||
       [ "$(test_path_metadata "$checked_release.image")" != 644:0:0 ] ||
       [ "$(test_path_metadata "$checked_release.attestation")" != 600:0:0 ] ||
       [ -e "$checked_release.login-uid" ] ||
       [ -L "$checked_release.login-uid" ]
    then
        echo "system install published incorrect release metadata" >&2
        exit 1
    fi
}

test_path_metadata()
{
    metadata_path=$1
    printf '%s:%s:%s\n' \
        "$(cclsh_stat_mode "$metadata_path")" \
        "$(cclsh_stat_owner "$metadata_path")" \
        "$(cclsh_stat_group "$metadata_path")"
}

test_paths_metadata()
{
    for metadata_path in "$@"; do
        printf '%s:%s:%s:%s\n' \
            "$(cclsh_stat_mode "$metadata_path")" \
            "$(cclsh_stat_owner "$metadata_path")" \
            "$(cclsh_stat_group "$metadata_path")" \
            "$metadata_path"
    done
}

fixture_bin=$temporary_directory/fixture-bin
mkdir "$fixture_bin"
real_getent=$(command -v getent)
cat >"$fixture_bin/getent" <<EOF
#!/bin/sh
case "\${1:-}" in
    passwd)
        printf 'root:x:0:0:root:/root:%s\n' "\$CCLSH_TEST_SHELL_PATH"
        printf 'nobody:x:$nobody_uid:$nobody_gid:nobody:/nonexistent:%s\n' \
            "\$CCLSH_TEST_SHELL_PATH"
        ;;
    group)
        printf '%s\n' 'root:x:0:daemon'
        ;;
    *)
        exec "$real_getent" "\$@"
        ;;
esac
EOF
chmod 755 "$fixture_bin/getent"

install_system_at()
{
    target_directory=$1
    target_shells_file=$2
    shift 2
    env \
        PATH="$fixture_bin:$PATH" \
        CCLSH_TEST_SHELL_PATH="$target_directory/cclsh" \
        CCLSH_SKIP_BUILD=1 \
        CCLSH_KERNEL_ARTIFACT="$source_shell" \
        CCLSH_IMAGE_ARTIFACT="$source_image" \
        CCLSH_INSTALL_DIRECTORY="$target_directory" \
        CCLSH_SYSTEM_SHELL=1 \
        CCLSH_PROBE_USER= \
        CCLSH_SHELLS_FILE="$target_shells_file" \
        CCLSH_BUILD_ATTESTATION="$attestation" \
        "$@" scripts/install
}

write_source_shell v1 none
printf '%s\n' 'system image v1' >"$source_image"
chmod 644 "$source_image"
write_test_attestation "$attestation" "$source_shell" "$source_image"
printf '%s\n' /bin/sh >"$shells_file"
chmod 640 "$shells_file"

prebuilt_refusal=$temporary_directory/prebuilt-refusal
set +e
CCLSH_CCL=$temporary_directory/missing-ccl \
CCLSH_SYSTEM_SHELL=1 \
CCLSH_INSTALL_DIRECTORY="$prebuilt_refusal" \
    scripts/install >"$temporary_directory/prebuilt.stdout" \
                    2>"$temporary_directory/prebuilt.stderr"
prebuilt_status=$?
set -e
if [ "$prebuilt_status" -ne 2 ] ||
   ! grep -Fqx \
       'cclsh install: system installation requires prebuilt attested artifacts; set CCLSH_SKIP_BUILD=1' \
       "$temporary_directory/prebuilt.stderr" ||
   [ -e "$prebuilt_refusal" ] || [ -L "$prebuilt_refusal" ]
then
    echo "system install did not refuse an implicit privileged build" >&2
    exit 1
fi

unsafe_directory=$temporary_directory/unsafe-bin
install -d -m 755 "$unsafe_directory"
chown "$nobody_uid:$nobody_gid" "$unsafe_directory"
if install_system_at "$unsafe_directory" "$shells_file" \
     >/dev/null 2>&1
then
    echo "system install accepted an untrusted destination" >&2
    exit 1
fi
if find "$unsafe_directory" -mindepth 1 -print | grep -q .; then
    echo "rejected system destination was modified" >&2
    exit 1
fi
noncanonical_directory=$temporary_directory/./noncanonical-bin
if install_system_at "$noncanonical_directory" "$shells_file" \
     >/dev/null 2>&1
then
    echo "system install accepted a noncanonical destination" >&2
    exit 1
fi
if [ -e "$temporary_directory/noncanonical-bin" ] ||
   [ -L "$temporary_directory/noncanonical-bin" ]
then
    echo "rejected noncanonical destination was modified" >&2
    exit 1
fi

install_system_at "$install_directory" "$shells_file" >/dev/null
release_one=$(cclsh_realpath_existing "$shell_path")
assert_system_release "$release_one"
if [ ! -L "$shell_path" ] || [ ! -L "$shell_path.image" ] ||
   [ "$(cclsh_stat_owner "$shell_path"):$(cclsh_stat_group "$shell_path")" != 0:0 ] ||
   [ "$(cclsh_stat_owner "$shell_path.image"):$(cclsh_stat_group "$shell_path.image")" != 0:0 ] ||
   [ "$(cclsh_realpath_existing "$shell_path.image")" != "$release_one.image" ] ||
   [ "$(grep -Fxc -- "$shell_path" "$shells_file")" -ne 1 ]
then
    echo "system install did not publish one stable shared shell" >&2
    exit 1
fi
"$run_as_user" nobody test -x "$release_one"
"$run_as_user" nobody test -r "$release_one.image"
"$run_as_user" nobody "$shell_path" --version >/dev/null
"$run_as_user" nobody "$shell_path" -c 'exit 0'

release_count=$(
    find "$install_directory/.cclsh-releases" \
        -mindepth 1 -maxdepth 1 -type d | wc -l
)
install_system_at "$install_directory" "$shells_file" \
    CCLSH_PROBE_USER=nobody >/dev/null
if [ "$(cclsh_realpath_existing "$shell_path")" != "$release_one" ] ||
   [ "$(
       find "$install_directory/.cclsh-releases" \
           -mindepth 1 -maxdepth 1 -type d | wc -l
     )" -ne "$release_count" ] ||
   [ "$(grep -Fxc -- "$shell_path" "$shells_file")" -ne 1 ]
then
    echo "probe choice changed shared release identity or registration" >&2
    exit 1
fi

install_system_at "$install_directory" "$shells_file" \
    CCLSH_LOGIN_USER=missing-legacy-user \
    CCLSH_PROBE_USER= >/dev/null
if [ "$(cclsh_realpath_existing "$shell_path")" != "$release_one" ]; then
    echo "an empty explicit probe selected the legacy login user" >&2
    exit 1
fi

failing_getent_bin=$temporary_directory/failing-getent-bin
mkdir "$failing_getent_bin"
cat >"$failing_getent_bin/getent" <<'EOF'
#!/bin/sh
exit 89
EOF
chmod 755 "$failing_getent_bin/getent"
install_system_at "$install_directory" "$shells_file" \
    PATH="$failing_getent_bin:$PATH" >/dev/null
if [ "$(cclsh_realpath_existing "$shell_path")" != "$release_one" ] ||
   [ "$(grep -Fxc -- "$shell_path" "$shells_file")" -ne 1 ]
then
    echo "system install still depends on passwd enumeration" >&2
    exit 1
fi

if command -v setpriv >/dev/null 2>&1; then
    alternate_group_directory=$temporary_directory/alternate-group-bin
    alternate_group_shells=$temporary_directory/alternate-group-shells
    printf '%s\n' /bin/sh >"$alternate_group_shells"
    chmod 640 "$alternate_group_shells"
    setpriv --egid "$nobody_gid" --clear-groups \
        env \
            PATH="$failing_getent_bin:$PATH" \
            CCLSH_SKIP_BUILD=1 \
            CCLSH_KERNEL_ARTIFACT="$source_shell" \
            CCLSH_IMAGE_ARTIFACT="$source_image" \
            CCLSH_INSTALL_DIRECTORY="$alternate_group_directory" \
            CCLSH_SYSTEM_SHELL=1 \
            CCLSH_PROBE_USER= \
            CCLSH_SHELLS_FILE="$alternate_group_shells" \
            CCLSH_BUILD_ATTESTATION="$attestation" \
            scripts/install >/dev/null
    alternate_group_release=$(
        cclsh_realpath_existing "$alternate_group_directory/cclsh"
    )
    assert_system_release "$alternate_group_release"
    if [ "$(cclsh_stat_owner "$alternate_group_directory/cclsh"):$(cclsh_stat_group "$alternate_group_directory/cclsh")" != 0:0 ] ||
       [ "$(cclsh_stat_owner "$alternate_group_directory/cclsh.image"):$(cclsh_stat_group "$alternate_group_directory/cclsh.image")" != 0:0 ] ||
       [ "$(grep -Fxc -- "$alternate_group_directory/cclsh" \
             "$alternate_group_shells")" -ne 1 ]
    then
        echo "system publication inherited the caller's effective group" >&2
        exit 1
    fi
fi

before_failed_probe_kernel=$(readlink "$shell_path")
before_failed_probe_image=$(readlink "$shell_path.image")
write_source_shell denied "$nobody_uid"
write_test_attestation "$attestation" "$source_shell" "$source_image"
if install_system_at "$install_directory" "$shells_file" \
     CCLSH_PROBE_USER=nobody >/dev/null 2>&1
then
    echo "system install accepted a failed optional-user probe" >&2
    exit 1
fi
if [ "$(readlink "$shell_path")" != "$before_failed_probe_kernel" ] ||
   [ "$(readlink "$shell_path.image")" != "$before_failed_probe_image" ] ||
   [ "$(grep -Fxc -- "$shell_path" "$shells_file")" -ne 1 ]
then
    echo "failed optional-user probe changed shared installation state" >&2
    exit 1
fi

write_source_shell v2 none
printf '%s\n' 'system image v2' >"$source_image"
write_test_attestation "$attestation" "$source_shell" "$source_image"
install_system_at "$install_directory" "$shells_file" >/dev/null
release_two=$(cclsh_realpath_existing "$shell_path")
assert_system_release "$release_two"
if [ "$release_two" = "$release_one" ] || [ ! -f "$release_one" ] ||
   [ "$(cclsh_realpath_existing "$shell_path.image")" != "$release_two.image" ] ||
   [ "$(grep -Fxc -- "$shell_path" "$shells_file")" -ne 1 ]
then
    echo "system update did not retain and activate a matched release" >&2
    exit 1
fi

reused_failure_marker=$temporary_directory/reused-probe-failure
cat >"$source_shell" <<EOF
#!/bin/sh
if test "\$(id -u)" = "$nobody_uid" &&
   test -e "$reused_failure_marker"
then
    exit 71
fi
case "\${1:-}" in
    --version|-c) exit 0 ;;
    *) exit 2 ;;
esac
EOF
chmod 755 "$source_shell"
printf '%s\n' 'reused system image' >"$source_image"
write_test_attestation "$attestation" "$source_shell" "$source_image"
install_system_at "$install_directory" "$shells_file" \
    CCLSH_PROBE_USER=nobody >/dev/null
reused_release=$(cclsh_realpath_existing "$shell_path")
assert_system_release "$reused_release"
reused_metadata=$(
    test_paths_metadata \
        "$(dirname "$reused_release")" \
        "$reused_release" \
        "$reused_release.image" \
        "$reused_release.attestation"
)
: >"$reused_failure_marker"
if install_system_at "$install_directory" "$shells_file" \
     CCLSH_PROBE_USER=nobody >/dev/null 2>&1
then
    echo "system install accepted a failed reused-release probe" >&2
    exit 1
fi
if [ "$(cclsh_realpath_existing "$shell_path")" != "$reused_release" ] ||
   [ "$(
       test_paths_metadata \
           "$(dirname "$reused_release")" \
           "$reused_release" \
           "$reused_release.image" \
           "$reused_release.attestation"
     )" != "$reused_metadata" ] ||
   [ "$(grep -Fxc -- "$shell_path" "$shells_file")" -ne 1 ]
then
    echo "failed reused-release probe changed shared metadata" >&2
    exit 1
fi
rm -f "$reused_failure_marker"

before_rollback_kernel=$(readlink "$shell_path")
before_rollback_image=$(readlink "$shell_path.image")
rollback_shells_copy=$temporary_directory/rollback-shells.expected
cp "$shells_file" "$rollback_shells_copy"
rollback_shells_mode=$(cclsh_stat_mode "$shells_file")
write_source_shell v3 none
printf '%s\n' 'system image v3' >"$source_image"
write_test_attestation "$attestation" "$source_shell" "$source_image"
if install_system_at "$install_directory" \
     "$temporary_directory/./shells" >/dev/null 2>&1
then
    echo "system install accepted a failed shell registration" >&2
    exit 1
fi
rollback_leftover=$(
    find "$install_directory" -maxdepth 1 \
        \( \( -type d -name '.cclsh-install.*' \) -o \
           \( -type d -name '.cclsh-transaction.*' \) -o \
           -name '.cclsh-rollback-*' \) \
        -print
)
if [ "$(readlink "$shell_path")" != "$before_rollback_kernel" ] ||
   [ "$(readlink "$shell_path.image")" != "$before_rollback_image" ] ||
   ! cmp -s "$rollback_shells_copy" "$shells_file" ||
   [ "$(cclsh_stat_mode "$shells_file")" != "$rollback_shells_mode" ] ||
   [ -n "$rollback_leftover" ]
then
    echo \
        "registration failure did not restore the shared installation:" \
        "$rollback_leftover" \
        >&2
    exit 1
fi

regular_directory=$temporary_directory/regular-bin
regular_shell=$regular_directory/cclsh
regular_image=$regular_directory/cclsh.image
regular_shell_copy=$temporary_directory/regular-cclsh.expected
regular_image_copy=$temporary_directory/regular-image.expected
regular_shells=$temporary_directory/regular-shells
regular_shells_copy=$temporary_directory/regular-shells.expected
install -d -m 755 "$regular_directory"
printf '%s\n' '#!/bin/sh' 'exit 71' >"$regular_shell"
printf '%s\n' 'previous regular image' >"$regular_image"
chmod 755 "$regular_shell"
chmod 644 "$regular_image"
cp "$regular_shell" "$regular_shell_copy"
cp "$regular_image" "$regular_image_copy"
printf '%s\n' /bin/sh >"$regular_shells"
chmod 640 "$regular_shells"
cp "$regular_shells" "$regular_shells_copy"
if install_system_at "$regular_directory" \
     "$temporary_directory/./regular-shells" >/dev/null 2>&1
then
    echo "regular-file rollback test unexpectedly registered" >&2
    exit 1
fi
regular_leftover=$(
    find "$regular_directory" -maxdepth 1 \
        \( \( -type d -name '.cclsh-install.*' \) -o \
           \( -type d -name '.cclsh-transaction.*' \) -o \
           -name '.cclsh-rollback-*' \) \
        -print
)
if [ -L "$regular_shell" ] || [ -L "$regular_image" ] ||
   ! cmp -s "$regular_shell_copy" "$regular_shell" ||
   ! cmp -s "$regular_image_copy" "$regular_image" ||
   ! cmp -s "$regular_shells_copy" "$regular_shells" ||
   [ -n "$regular_leftover" ]
then
    echo \
        "registration failure did not restore regular files:" \
        "$regular_leftover" \
        >&2
    exit 1
fi

owner_shells=$temporary_directory/owner-shells
printf '%s\n' /bin/sh >"$owner_shells"
chmod 640 "$owner_shells"
if CCLSH_SKIP_BUILD=1 \
   CCLSH_KERNEL_ARTIFACT="$source_shell" \
   CCLSH_IMAGE_ARTIFACT="$source_image" \
   CCLSH_INSTALL_DIRECTORY="$install_directory" \
   CCLSH_SHELLS_FILE="$owner_shells" \
   scripts/install >/dev/null 2>&1
then
    echo "owner-only install replaced a system-managed shell" >&2
    exit 1
fi
if [ "$(readlink "$shell_path")" != "$before_rollback_kernel" ] ||
   [ "$(readlink "$shell_path.image")" != "$before_rollback_image" ]
then
    echo "rejected owner-only install changed the system shell" >&2
    exit 1
fi

# Construct the release shape produced by the former UID-bound installer.
write_source_shell legacy none
printf '%s\n' 'legacy target image' >"$source_image"
write_test_attestation "$attestation" "$source_shell" "$source_image"
legacy_directory=$temporary_directory/legacy-bin
legacy_releases=$legacy_directory/.cclsh-releases
legacy_release=$legacy_releases/legacy-target/cclsh
legacy_shell=$legacy_directory/cclsh
legacy_shells=$temporary_directory/legacy-shells
install -d -m 755 "$legacy_directory" "$legacy_releases" \
    "$(dirname "$legacy_release")"
install -m 755 "$source_shell" "$legacy_release"
install -m 640 "$source_image" "$legacy_release.image"
chgrp "$nobody_gid" "$legacy_release.image"
install -m 600 "$attestation" "$legacy_release.attestation"
printf '%s\n' "$nobody_uid" >"$legacy_release.login-uid"
chmod 600 "$legacy_release.login-uid"
ln -s .cclsh-releases/legacy-target/cclsh "$legacy_shell"
ln -s .cclsh-releases/legacy-target/cclsh.image "$legacy_shell.image"
printf '%s\n' /bin/sh "$legacy_shell" >"$legacy_shells"
chmod 640 "$legacy_shells"

legacy_metadata=$(
    test_paths_metadata \
        "$(dirname "$legacy_release")" \
        "$legacy_release" \
        "$legacy_release.image" \
        "$legacy_release.attestation" \
        "$legacy_release.login-uid"
)
legacy_hashes=$(
    scripts/file-sha256 "$legacy_release"
    scripts/file-sha256 "$legacy_release.image"
    scripts/file-sha256 "$legacy_release.attestation"
    scripts/file-sha256 "$legacy_release.login-uid"
)

legacy_owner_shells=$temporary_directory/legacy-owner-shells
printf '%s\n' /bin/sh >"$legacy_owner_shells"
chmod 640 "$legacy_owner_shells"
if CCLSH_SKIP_BUILD=1 \
   CCLSH_KERNEL_ARTIFACT="$source_shell" \
   CCLSH_IMAGE_ARTIFACT="$source_image" \
   CCLSH_INSTALL_DIRECTORY="$legacy_directory" \
   CCLSH_SHELLS_FILE="$legacy_owner_shells" \
   scripts/install >/dev/null 2>&1
then
    echo "owner-only install replaced a legacy login-managed shell" >&2
    exit 1
fi
if [ "$(cclsh_realpath_existing "$legacy_shell")" != "$legacy_release" ]; then
    echo "rejected owner-only install changed the legacy shell" >&2
    exit 1
fi

install_system_at "$legacy_directory" "$legacy_shells" \
    CCLSH_PROBE_USER=nobody >/dev/null
migrated_release=$(cclsh_realpath_existing "$legacy_shell")
assert_system_release "$migrated_release"
if [ "$migrated_release" = "$legacy_release" ] ||
   [ "$(cclsh_realpath_existing "$legacy_shell.image")" != \
     "$migrated_release.image" ] ||
   [ "$(grep -Fxc -- "$legacy_shell" "$legacy_shells")" -ne 1 ] ||
   [ "$(
       test_paths_metadata \
           "$(dirname "$legacy_release")" \
           "$legacy_release" \
           "$legacy_release.image" \
           "$legacy_release.attestation" \
           "$legacy_release.login-uid"
     )" != "$legacy_metadata" ] ||
   [ "$(
       scripts/file-sha256 "$legacy_release"
       scripts/file-sha256 "$legacy_release.image"
       scripts/file-sha256 "$legacy_release.attestation"
       scripts/file-sha256 "$legacy_release.login-uid"
     )" != "$legacy_hashes" ]
then
    echo "system install did not safely migrate the legacy release" >&2
    exit 1
fi
"$run_as_user" nobody test -x "$migrated_release"
"$run_as_user" nobody test -r "$migrated_release.image"
"$run_as_user" nobody "$legacy_shell" --version >/dev/null

before_compatibility_release=$migrated_release
env \
    PATH="$fixture_bin:$PATH" \
    CCLSH_TEST_SHELL_PATH="$legacy_shell" \
    CCLSH_SKIP_BUILD=1 \
    CCLSH_KERNEL_ARTIFACT="$source_shell" \
    CCLSH_IMAGE_ARTIFACT="$source_image" \
    CCLSH_INSTALL_DIRECTORY="$legacy_directory" \
    CCLSH_LOGIN_USER=nobody \
    CCLSH_SHELLS_FILE="$legacy_shells" \
    CCLSH_BUILD_ATTESTATION="$attestation" \
    scripts/install >/dev/null
if [ "$(cclsh_realpath_existing "$legacy_shell")" != \
     "$before_compatibility_release" ] ||
   [ -e "$before_compatibility_release.login-uid" ] ||
   [ -L "$before_compatibility_release.login-uid" ] ||
   [ "$(grep -Fxc -- "$legacy_shell" "$legacy_shells")" -ne 1 ]
then
    echo "compatibility login install restored per-user binding" >&2
    exit 1
fi

if CCLSH_SKIP_BUILD=1 \
   CCLSH_KERNEL_ARTIFACT="$source_shell" \
   CCLSH_IMAGE_ARTIFACT="$source_image" \
   CCLSH_INSTALL_DIRECTORY="$legacy_directory" \
   CCLSH_SHELLS_FILE="$legacy_owner_shells" \
   scripts/install >/dev/null 2>&1
then
    echo "owner-only install replaced a migrated system shell" >&2
    exit 1
fi
if [ "$(cclsh_realpath_existing "$legacy_shell")" != "$migrated_release" ]; then
    echo "rejected owner-only install changed the migrated shell" >&2
    exit 1
fi

echo "Shared system-shell installation checks passed."
