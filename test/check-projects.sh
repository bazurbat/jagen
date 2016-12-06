#!/bin/sh

set -eu

work_dir="$PWD"
test_dir=$(dirname "$0")
init_project=$test_dir/../init-project
templates=$($init_project -l)

check_project() {
    local name="$1"
    cd "$name"
    echo "=== $name ==="
    diff -u build.ninja.orig build/build.ninja || true
    diff -u include.orig include || true
    cd - >/dev/null
}

cd "$work_dir"

for t in $templates; do
    check_project $t
done

