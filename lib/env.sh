#!/bin/sh

export pkg_bin_dir="$pkg_root/bin"
export pkg_lib_dir="$pkg_root/lib"
export pkg_src_dir="$pkg_root/src"

export pkg_bin="chibi-scheme -r $pkg_lib_dir/jagen.scm"
export pkg_debug="no"

export pkg_sdk="sigma"
export pkg_flags=""
export pkg_build_type="Release"
export pkg_build_verbose="no"
export pkg_source_exclude=""

. "$pkg_lib_dir/common.sh" ||
    { echo "Failed to load common functions"; exit 1; }

include "${HOME}/.config/jagen/env"
include "$pkg_root/local"

: ${pkg_private_dir:="$pkg_src_dir/files"}
: ${pkg_dist_dir:="$pkg_root/dist/$pkg_sdk"}
: ${pkg_patch_dir:="$pkg_dist_dir/patches"}
: ${pkg_build_dir:="$pkg_root/build"}
: ${pkg_build_include_dir:="$pkg_build_dir/include"}

export pkg_private_dir pkg_dist_dir pkg_patch_dir
export pkg_build_dir pkg_build_include_dir

include "$pkg_lib_dir/env/cmake"
include "$pkg_lib_dir/env/sdk"
