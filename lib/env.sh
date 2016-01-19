#!/bin/sh

# deal with it
if [ "$ZSH_VERSION" ]; then
    setopt shwordsplit
fi

export jagen_shell=""

export jagen_debug="${jagen_debug}"
export jagen_flags=""
export jagen_sdk=""
export jagen_overlays

export jagen_lib_dir="$jagen_dir/lib"
export jagen_src_dir="$jagen_root/src"
export jagen_build_dir="$jagen_root/build"
export jagen_include_dir="$jagen_root/include"
export jagen_log_dir="$jagen_build_dir"
export jagen_output_dir="$jagen_root/out"

jagen_build_type="Release"
jagen_build_verbose="no"

. "$jagen_lib_dir/common.sh" || return

# Avoid import during init-root
if [ "$jagen_root" ]; then
    try_include "$jagen_root/config.sh"
fi

jagen_cmake_generator="${jagen_cmake_generator:-Ninja}"
jagen_cmake_build_options="${jagen_cmake_build_options}"
jagen_cmake_build_type="$jagen_build_type"

export jagen_patch_dir="$jagen_dist_dir/patches"
export jagen_private_dir="$jagen_src_dir/files"

jagen_host_dir="$jagen_root/host"
jagen_host_prefix=""

jagen_target_dir="$jagen_root/target"
jagen_target_prefix="/usr"

jagen_tools_dir="$jagen_root/tools"
jagen_tools_prefix=""

export LUA_PATH="$jagen_lib_dir/?.lua;;"

add_PATH "$jagen_host_dir/bin"
add_LD_LIBRARY_PATH "$jagen_host_dir/lib"

export PATH
export LD_LIBRARY_PATH
export LINGUAS=""

in_flags ccache && use_env ccache

for overlay in $jagen_overlays; do
    try_include "$jagen_dir/overlay/$overlay/env.sh"
done
