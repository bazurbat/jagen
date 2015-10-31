#!/bin/sh

# deal with it
if [ "$ZSH_VERSION" ]; then
    setopt shwordsplit
fi

export jagen_shell=""

export jagen_debug=""
export jagen_flags=""
export jagen_sdk=""

export jagen_bin_dir="$jagen_dir/bin"
export jagen_lib_dir="$jagen_dir/lib"
export jagen_src_dir="$jagen_root/src"
export jagen_build_dir="$jagen_root/build"
export jagen_include_dir="$jagen_root/include"
export jagen_log_dir="$jagen_root/log"

jagen_build_type="Release"
jagen_build_verbose="no"

. "$jagen_lib_dir/common.sh" || return

if [ "$XDG_CONFIG_HOME" ]; then
    try_include "$XDG_CONFIG_HOME/jagen/env" || return
else
    try_include "$HOME/.config/jagen/env" || return
fi
try_include "$jagen_root/config.sh" || return

jagen_cmake_generator="${jagen_cmake_generator:-Ninja}"
jagen_cmake_build_options="${jagen_cmake_build_options}"
jagen_cmake_build_type="$jagen_build_type"

export jagen_patch_dir="$jagen_dist_dir/patches"
export jagen_private_dir="$jagen_src_dir/files"

jagen_host_dir="$jagen_root/host"
jagen_host_prefix=""

jagen_target_dir="$jagen_root/target"
jagen_target_prefix=""

jagen_tools_dir="$jagen_root/tools"
jagen_tools_prefix=""

jagen_install_dir="$jagen_root/firmware"

add_PATH "$jagen_host_dir/bin"
add_LD_LIBRARY_PATH "$jagen_host_dir/lib"

export PATH
export LD_LIBRARY_PATH
export LINGUAS=""

in_flags ccache && use_env ccache
use_env sdk || return
