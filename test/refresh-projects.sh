#!/bin/sh

set -eu

work_dir="$PWD"
test_dir=$(dirname "$0")
init_project=$test_dir/../init-project
templates=$($init_project -l)

refresh_project() {
    local name="$1"
    cd "$name"
    ( . ./env.sh && jagen refresh )
    cd - >/dev/null
}

cd "$work_dir"

for t in $templates; do
    refresh_project $t
done
