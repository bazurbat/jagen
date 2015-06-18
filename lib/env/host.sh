#!/bin/sh

export host_dir="$pkg_build_dir/host"
export host_prefix=""

p_path_prepend "$host_dir/bin"
p_ld_library_path_prepend "$host_dir/lib"

export PATH
export LD_LIBRARY_PATH
