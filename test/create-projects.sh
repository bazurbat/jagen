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

cd "$work_dir"

for t in $templates; do
    create_project $t
done
