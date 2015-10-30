#!/bin/sh

# deal with it
if [ "$ZSH_VERSION" ]; then
    setopt shwordsplit
fi

export pkg_shell=""

export pkg_debug=""
export pkg_flags=""
export pkg_sdk=""

export jagen_bin_dir="$jagen_dir/bin"
export jagen_lib_dir="$jagen_dir/lib"
export jagen_src_dir="$jagen_root/src"
export jagen_build_dir="$jagen_root/build"

pkg_build_type="Release"
pkg_build_verbose="no"

. "$jagen_lib_dir/common.sh" || return

if [ "$XDG_CONFIG_HOME" ]; then
    try_include "$XDG_CONFIG_HOME/jagen/env" || return
else
    try_include "$HOME/.config/jagen/env" || return
fi
try_include "$jagen_root/local.sh" || return

export jagen_patch_dir="$pkg_dist_dir/patches"
export jagen_build_include_dir="$pkg_build_dir/include"
export jagen_private_dir="$pkg_src_dir/files"

cmake_generator="${cmake_generator:-Ninja}"
cmake_build_options="${cmake_build_options}"
cmake_build_type="$pkg_build_type"

install_dir="$jagen_build_dir/firmware"

host_dir="$jagen_build_dir/host"
host_prefix=""

target_dir="$jagen_build_dir/target"
target_prefix=""

tools_dir="$jagen_build_dir/tools"
tools_prefix=""

add_PATH "$host_dir/bin"
add_LD_LIBRARY_PATH "$host_dir/lib"

export PATH
export LD_LIBRARY_PATH
export LINGUAS=""

in_flags ccache && use_env ccache
use_env sdk || return
