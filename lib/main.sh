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

export jagen_bin_dir="$jagen_root/bin"
export jagen_src_dir="$jagen_root/src"
export jagen_build_dir="$jagen_root/build"
export jagen_include_dir="$jagen_root/include"
export jagen_log_dir="$jagen_build_dir"

jagen_product=""
jagen_sdk=""
jagen_toolchain=""
jagen_vendor=""

jagen_path="$jagen_dir/lib"
export LUA_PATH="$jagen_dir/lib/?.lua;$jagen_dir/src/?.lua;;"

export jagen_toolchain_dir
export jagen_sdk_dir

jagen_build_verbose="no"

. "$jagen_lib_dir/common.sh" || return

# Avoid import during init-root
if [ "$jagen_root" ]; then
    include "$jagen_root/config"
fi

if [ "$jagen_product" ]; then
    export jagen_product_dir="$jagen_dir/usr/product/$jagen_product"
    include "$jagen_product_dir/init"
fi

if [ "$jagen_vendor" ]; then
    jagen_path="$jagen_dir/usr/vendor/$jagen_vendor $jagen_path"
    LUA_PATH="$jagen_dir/usr/vendor/$jagen_vendor/?.lua;$LUA_PATH"
fi

if [ "$jagen_toolchain" ]; then
    jagen_path="$jagen_dir/usr/toolchain/$jagen_toolchain $jagen_path"
    LUA_PATH="$jagen_dir/usr/toolchain/$jagen_toolchain/?.lua;$LUA_PATH"
fi

if [ "$jagen_sdk" ]; then
    jagen_path="$jagen_dir/usr/sdk/$jagen_sdk $jagen_path"
    LUA_PATH="$jagen_dir/usr/sdk/$jagen_sdk/?.lua;$LUA_PATH"
fi

if [ "$jagen_product_dir" ]; then
    jagen_path="$jagen_product_dir $jagen_path"
    LUA_PATH="$jagen_product_dir/?.lua;$LUA_PATH"
fi

export jagen_host_dir="$jagen_root/host"

export jagen_target_dir="$jagen_root/target"

add_PATH "$jagen_host_dir/bin"
add_LD_LIBRARY_PATH "$jagen_host_dir/lib"

export PATH
export LD_LIBRARY_PATH
export LINGUAS=""

in_flags ccache && using ccache

import env || die

return 0
