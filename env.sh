#!/bin/sh

# We are trying to carefully handle the field splitting according to POSIX
# behaviour, zsh by default do not split on whitespace which interferes with
# that.
if [ "${ZSH_VERSION-}" ]; then
    setopt shwordsplit
fi

jagen_S="$(printf '\n!')"; jagen_S=${jagen_S%!}
jagen_FS="$(printf '\t')"
jagen_IFS="$(printf '\n\t')"

# These globals are coming from root's env.sh
jagen_dir="${jagen_dir:?}"
jagen_root_dir="$jagen_root_dir"

jagen_shell=""
jagen_lua="${jagen_lua-}"
: ${jagen_lua:=$(command -v luajit)}
: ${jagen_lua:=$(command -v lua)}

jagen_debug="${jagen_debug-}"
jagen_flags=""

jagen_lib_dir="$jagen_dir/lib"
jagen__src_dir="$jagen_dir/src"
jagen_root_lib_dir="$jagen_root_dir/lib"

jagen_bin_dir="$jagen_root_dir/bin"
jagen_src_dir="$jagen_root_dir/src"
jagen_dist_dir="$jagen_root_dir/dist"
jagen_build_dir="$jagen_root_dir/build"
jagen_include_dir="$jagen_root_dir/include"
jagen_log_dir="$jagen_root_dir/log"

jagen_build_verbose=${jagen_build_verbose-}

. "$jagen_dir/src/common.sh" || return

# Avoid import during init-root
if [ "$jagen_root_dir" ] && [ -f "$jagen_root_dir/config.sh" ]; then
    include "$jagen_root_dir/config"
fi

jagen_host_dir="$jagen_root_dir/host"
jagen_target_dir="$jagen_root_dir/target"
jagen_cargo_config_dir="$jagen_root_dir/.cargo"

add_PATH "$jagen_host_dir/bin"
add_LD_LIBRARY_PATH "$jagen_host_dir/lib"
export PATH LD_LIBRARY_PATH

# this should come from rustup package but is not configurable yet
export RUSTUP_HOME="$jagen_dist_dir/rustup"
export CARGO_HOME="$jagen_dist_dir/cargo"

jagen__set_path

import env || true # it is OK if no env was found

# May be set in layers and not exist during the env sourcing.
if [ "${jagen_private_dir-}" ]; then
    add_PATH "$jagen_private_dir/bin"
fi

# export all jagen_* variables
for var in $(set | jagen_esed -n 's/^(jagen_[[:alnum:]][[:alnum:]_]*)=.*/\1/p'); do
    export $var
done; unset var
