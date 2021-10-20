#!/bin/sh

jagen_S="$(printf '\n!')"; jagen_S=${jagen_S%!}
jagen_FS="$(printf '\t')"
jagen_IFS="$(printf '\n\t')"

# These globals are coming from root's env.sh
jagen_dir="${jagen_dir:?}"
jagen_root_dir="$jagen_root_dir"

jagen_lua="${jagen_lua-}"
: ${jagen_lua:=$(command -v luajit)}
: ${jagen_lua:=$(command -v lua)}

jagen_debug="${jagen_debug-}"

jagen__src_dir="$jagen_dir/src"
jagen_root_lib_dir="$jagen_root_dir/lib"

jagen_build_dir="$jagen_root_dir/build"
jagen_include_dir="$jagen_build_dir/include"

jagen_build_verbose=${jagen_build_verbose-}

. "$jagen_dir/src/common.sh" || return

jagen__set_path || return

import env || true # it is OK if no env was found

# export all jagen_* variables
for var in $(set | LC_ALL=C sed -n 's/^\(jagen_[[:alnum:]][[:alnum:]_]*\)=.*/\1/p'); do
    export "$var"
done; unset var
