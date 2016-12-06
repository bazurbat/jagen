#!/bin/sh

set -eu

work_dir="$PWD"
test_dir=$(dirname "$0")
init_project=$test_dir/../init-project
templates=$($init_project -l)

create_project() {
    local name="$1"
    mkdir -p "$name"
    cd "$name"
    "../$init_project" -f "$name"
    cp -f build/build.ninja build.ninja.orig
    cp -a include/ include.orig
    cd - >/dev/null
}

refresh_project() {
    local name="$1"
    cd "$name"
    ( . ./env.sh && jagen refresh )
    cd - >/dev/null
}

check_project() {
    local name="$1"
    cd "$name"
    echo "=== $name ==="
    diff -u build.ninja.orig build/build.ninja || true
    diff -u include.orig include || true
    cd - >/dev/null
}

cd "$work_dir"

if [ -z "${1-}" ]; then
    cat <<EOF
Usage: $0 [setup|refresh|check|cleanup]
EOF
    exit 0
fi

case $1 in
    setup)
        for t in $templates; do
            create_project $t
        done ;;
    refresh)
        for t in $templates; do
            refresh_project $t
        done ;;
    check)
        for t in $templates; do
            check_project $t
        done ;;
    cleanup)
        for t in $templates; do
            rm -rf "$t"
        done ;;
esac
