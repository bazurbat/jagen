#!/bin/sh

export host_dir="$pkg_build_dir/host"
export host_prefix=""

add_PATH "$host_dir/bin"
add_LD_LIBRARY_PATH "$host_dir/lib"

export PATH
export LD_LIBRARY_PATH
