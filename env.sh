#!/bin/sh

export pkg_root=$(realpath .)

. "$pkg_root/lib/env.sh" ||
    { echo "Failed to load environment"; exit 1; }

p_path_prepend "$target_bin_dir"
p_path_prepend "$pkg_private_dir/bin"
p_path_prepend "$pkg_root/bin"
