#!/bin/sh

# We are trying to carefully handle the field splitting according to POSIX
# behaviour, zsh by default do not split on whitespace which interferes with
# that.
if [ "${ZSH_VERSION-}" ]; then
    setopt shwordsplit
fi

export jagen_S="$(printf '\n!')"; jagen_S=${jagen_S%!}
export jagen_FS="$(printf '\t')"
export jagen_IFS="$(printf '\n\t')"

# These globals are coming from project's env.sh
export jagen_dir="${jagen_dir:?}"
export jagen_project_dir="$jagen_project_dir"

export jagen_shell=""
export jagen_lua="${jagen_lua-}"
: ${jagen_lua:=$(test "$(command -v luajit)" && echo luajit)}
: ${jagen_lua:=$(test "$(command -v lua)" && echo lua)}

export jagen_debug="${jagen_debug-}"
export jagen_flags=""

export jagen_lib_dir="$jagen_dir/lib"
export jagen_project_lib_dir="$jagen_project_dir/lib"

export jagen_bin_dir="$jagen_project_dir/bin"
export jagen_src_dir="$jagen_project_dir/src"
export jagen_dist_dir="$jagen_project_dir/dist"
export jagen_toolchains_dir="$jagen_project_dir/toolchains"
export jagen_build_dir="$jagen_project_dir/build"
export jagen_include_dir="$jagen_project_dir/include"
export jagen_log_dir="$jagen_build_dir"

. "$jagen_dir/src/common.sh" || return

# Avoid import during init-root
if [ "$jagen_project_dir" ]; then
    include "$jagen_project_dir/config"
fi

export jagen_host_dir="$jagen_project_dir/host"
export jagen_target_dir="$jagen_project_dir/target"

add_PATH "$jagen_host_dir/bin"
add_LD_LIBRARY_PATH "$jagen_host_dir/lib"
export PATH LD_LIBRARY_PATH

export jagen_layers
export jagen_sdk
export jagen_source_exclude
export jagen_target_board
export jagen_target_toolchain

jagen__set_path

import env || true # it is OK if no env was found

# May be set in layers and not exist during the env sourcing.
if [ "${jagen_private_dir-}" ]; then
    export jagen_private_dir
    add_PATH "$jagen_private_dir/bin"
fi
