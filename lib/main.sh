#!/bin/sh

# deal with it
if [ "$ZSH_VERSION" ]; then
    setopt shwordsplit
fi

jagen_FS=$(printf '\t')
jagen_IFS=$(printf '\n\t')

export jagen_shell=""

export jagen_debug="${jagen_debug}"
export jagen_flags=""

export jagen_lib_dir="${jagen_dir:?}/lib"

export jagen_bin_dir="$jagen_project_dir/bin"
export jagen_src_dir="$jagen_project_dir/src"
export jagen_dist_dir="$jagen_project_dir/dist"
export jagen_toolchains_dir="$jagen_project_dir/toolchains"
export jagen_build_dir="$jagen_project_dir/build"
export jagen_include_dir="$jagen_project_dir/include"
export jagen_log_dir="$jagen_build_dir"
export jagen_private_dir

export jagen_path="$jagen_dir/lib"
export LUA_PATH="$jagen_dir/lib/?.lua;$jagen_dir/src/?.lua;;"

jagen_build_verbose="no"

. "$jagen_lib_dir/common.sh" || return

# Avoid import during init-root
if [ "$jagen_project_dir" ]; then
    include "$jagen_project_dir/config"
fi

set_jagen_path() {
    local path IFS="$jagen_IFS" FS="$jagen_FS"

    for path in $jagen_layers; do
        jagen_path="$path${FS}$jagen_path"
        LUA_PATH="$path/?.lua;$LUA_PATH"
    done
}
set_jagen_path

export jagen_host_dir="$jagen_project_dir/host"
export jagen_target_dir="$jagen_project_dir/target"

add_PATH "$jagen_host_dir/bin"
add_LD_LIBRARY_PATH "$jagen_host_dir/lib"

export jagen_sdk
export jagen_target_toolchain
export jagen_target_board

export PATH
export LD_LIBRARY_PATH
export LINGUAS=""

in_flags ccache && use_env ccache

import env || die
require toolchain || die

return 0
