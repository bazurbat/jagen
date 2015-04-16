#!/bin/sh

export jagen_root=$(realpath .)

. "$jagen_root/lib/env.sh" ||
    { echo "Failed to load environment"; exit 1; }

use_env jagen

p_path_prepend "$target_bin_dir"
p_path_prepend "$pkg_private_dir/bin"
p_path_prepend "$jagen_root/bin"
