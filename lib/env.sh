#!/bin/sh

# deal with it
if [ "$ZSH_VERSION" ]; then
    setopt shwordsplit
fi

export pkg_bin_dir="$jagen_root/bin"
export pkg_lib_dir="$jagen_root/lib"

export pkg_debug=""

export pkg_flags=""
export pkg_sdk=""
pkg_source_exclude=""

export pkg_build_dir="$jagen_build_root/build"
pkg_build_type="Release"
pkg_build_verbose="no"

export pkg_src_dir="$jagen_build_root/src"

. "$pkg_lib_dir/common.sh" || return

if [ "$XDG_CONFIG_HOME" ]; then
    try_include "$XDG_CONFIG_HOME/jagen/env" || return
else
    try_include "$HOME/.config/jagen/env" || return
fi
try_include "$jagen_build_root/local.sh" || return

export pkg_patch_dir="$pkg_dist_dir/patches"
export pkg_build_include_dir="$pkg_build_dir/include"
export pkg_private_dir="$pkg_src_dir/files"

in_flags ccache && use_env ccache
use_env cmake  || return
use_env host   || return
use_env target || return
use_env sdk    || return
