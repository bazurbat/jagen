#!/bin/sh

export pkg_root=$(realpath .)

. "$pkg_root/lib/env.sh" ||
    { echo "Failed to load environment"; exit 1; }

PATH="$pkg_root/bin:$pkg_private_dir/bin:$target_bin_dir:$PATH"
