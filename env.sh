#!/bin/sh

export pkg_root=$(realpath .)

export pkg_bin_dir="$pkg_root/bin"
export pkg_lib_dir="$pkg_root/lib"
export pkg_src_dir="$pkg_root/src"

export pkg_bin="chibi-scheme -r $pkg_lib_dir/jagen.scm"

export pkg_sdk="sigma"
export pkg_build_type="Release"

export pkg_build_dir="$pkg_root/build"
export pkg_dist_dir="$pkg_root/dist/$pkg_sdk"
export pkg_private_dir="$pkg_src_dir/files"

PATH="$pkg_root/bin:$pkg_private_dir/bin:$PATH"
