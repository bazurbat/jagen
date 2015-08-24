#!/bin/sh

# deal with it
if [ "$ZSH_VERSION" ]; then
    setopt shwordsplit
fi

pkg_bin_dir="$jagen_root/bin"
pkg_lib_dir="$jagen_root/lib"

pkg_debug=""

pkg_flags=""
pkg_sdk=""
pkg_source_exclude=""

pkg_build_dir="$jagen_build_root/build"
pkg_build_type="Release"
pkg_build_verbose="no"

pkg_src_dir="$jagen_build_root/src"

. "$pkg_lib_dir/common.sh" ||
    { echo "Failed to load common library"; return 1; }

if [ "$XDG_CONFIG_HOME" ]; then
    try_include "$XDG_CONFIG_HOME/jagen/env"
else
    try_include "$HOME/.config/jagen/env"
fi
try_include "$jagen_build_root/local.sh"

pkg_patch_dir="$pkg_dist_dir/patches"
pkg_build_include_dir="$pkg_build_dir/include"
pkg_private_dir="$pkg_src_dir/files"

in_flags ccache && use_env ccache
include "$pkg_lib_dir/env/cmake" || return
include "$pkg_lib_dir/env/sdk"   || return
use_env host
