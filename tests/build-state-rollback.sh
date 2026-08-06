#!/bin/sh
# Check that failed publication commands restore every public build path.
set -eu
cd "$(dirname "$0")/.."

temporary_directory=$(
    mktemp -d "${TMPDIR:-/tmp}/cclsh-state-rollback-check.XXXXXX"
)
cleanup()
{
    rm -rf -- "$temporary_directory"
}
trap cleanup 0
trap 'exit 129' 1
trap 'exit 130' 2
trap 'exit 143' 15

project=$temporary_directory/project
mkdir -p "$project/scripts" "$project/releases/old"
cp scripts/platform scripts/with-build-state-rollback "$project/scripts/"
printf '%s\n' '#!/bin/sh' 'exit 0' >"$project/scripts/build-lock-held"
chmod 755 \
    "$project/scripts/with-build-state-rollback" \
    "$project/scripts/build-lock-held"

printf '%s\n' old-kernel >"$project/releases/old/cclsh"
printf '%s\n' old-image >"$project/releases/old/cclsh.image"
printf '%s\n' old-attestation >"$project/cclsh.attestation"
ln -s releases/old/cclsh "$project/cclsh"
ln -s releases/old/cclsh.image "$project/cclsh.image"
cp "$project/cclsh.attestation" "$project/attestation.expected"

set +e
(
    cd "$project"
    scripts/with-build-state-rollback sh -c '
        rm -f cclsh cclsh.image cclsh.attestation
        printf "%s\n" new-kernel >cclsh
        printf "%s\n" new-image >cclsh.image
        printf "%s\n" new-attestation >cclsh.attestation
        exit 71
    '
)
rollback_status=$?
set -e
if [ "$rollback_status" -ne 71 ] ||
   [ "$(readlink "$project/cclsh")" != releases/old/cclsh ] ||
   [ "$(readlink "$project/cclsh.image")" != releases/old/cclsh.image ] ||
   ! cmp -s "$project/attestation.expected" "$project/cclsh.attestation" ||
   find "$project" -maxdepth 1 -name '.cclsh-state-transaction.*' \
       -print | grep -q .
then
    echo "failed build command did not restore public state" >&2
    exit 1
fi

rm -f "$project/cclsh.attestation"
set +e
(
    cd "$project"
    scripts/with-build-state-rollback sh -c '
        printf "%s\n" temporary >cclsh.attestation
        exit 72
    '
)
missing_status=$?
set -e
if [ "$missing_status" -ne 72 ] ||
   [ -e "$project/cclsh.attestation" ] ||
   [ -L "$project/cclsh.attestation" ]
then
    echo "failed build command did not restore a missing attestation" >&2
    exit 1
fi

printf '%s\n' old-attestation >"$project/cclsh.attestation"
cp "$project/cclsh.attestation" "$project/attestation.expected"
set +e
(
    cd "$project"
    scripts/with-build-state-rollback sh -c '
        printf "%s\n" in-place-mutation >cclsh.attestation
        exit 73
    '
)
mutation_status=$?
set -e
if [ "$mutation_status" -ne 73 ] ||
   ! cmp -s "$project/attestation.expected" "$project/cclsh.attestation" ||
   find "$project" -maxdepth 1 -name '.cclsh-state-transaction.*' \
       -print | grep -q .
then
    echo "rollback snapshot did not survive in-place mutation" >&2
    exit 1
fi

set +e
(
    cd "$project"
    scripts/with-build-state-rollback sh -c 'exit 74'
)
unchanged_status=$?
set -e
if [ "$unchanged_status" -ne 74 ] ||
   ! cmp -s "$project/attestation.expected" "$project/cclsh.attestation" ||
   find "$project" -maxdepth 1 -name '.cclsh-state-transaction.*' \
       -print | grep -q .
then
    echo "rollback failed when public regular state was unchanged" >&2
    exit 1
fi

if scripts/login-build "$temporary_directory" ccl \
     "$temporary_directory/custom.attestation" >/dev/null 2>&1
then
    echo "login-build accepted a nontransactional attestation path" >&2
    exit 1
fi

echo "Build-state rollback checks passed."
