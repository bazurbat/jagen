#!/bin/sh

export chibi_dir="$pkg_build_dir/chibi"

p_path_prepend "$chibi_dir/bin"
p_ld_library_path_prepend "$chibi_dir/lib"

export PATH
export LD_LIBRARY_PATH
