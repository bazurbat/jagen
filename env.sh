#!/bin/sh

run_refresh="yes"
[ "$1" = "--no-refresh" ] && run_refresh="no"

export pkg_root=$(realpath .)

. "$pkg_root/lib/env.sh" ||
    { echo "Failed to load environment"; exit 1; }

PATH="$pkg_root/bin:$pkg_private_dir/bin:$PATH"

[ "$run_refresh" = "yes" ] && jagen refresh
