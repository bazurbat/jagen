#!/bin/sh

export host_dir="$pkg_build_dir/host"
export host_prefix=""

export PATH="$host_dir/bin:$PATH"
export LD_LIBRARY_PATH="$host_dir/lib:$LD_LIBRARY_PATH"

if in_flags ccache; then
    p_path_prepend "$jagen_ccache_bin_dir"
fi
