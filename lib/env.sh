#!/bin/sh

export pkg_bin_dir="$jagen_root/bin"
export pkg_lib_dir="$jagen_root/lib"
export pkg_src_dir="$jagen_root/src"

export pkg_debug=""

export pkg_flags=""
export pkg_sdk=""
export pkg_source_exclude=""

export pkg_build_dir="$jagen_root/build"
export pkg_build_type="Release"
export pkg_build_verbose="no"

jagen_try_include() { [ -f "$1" ] && . "$1"; }

. "$pkg_lib_dir/list.sh" ||
    { echo "Failed to load list library"; exit 1; }
. "$pkg_lib_dir/common.sh" ||
    { echo "Failed to load common library"; exit 1; }

export LUA_PATH="${pkg_lib_dir}/?.lua;;;"

_jagen() {
    lua "$pkg_lib_dir/jagen.lua" "$@"
}

if [ "$XDG_CONFIG_HOME" ]; then
    jagen_try_include "$XDG_CONFIG_HOME/jagen/env"
else
    jagen_try_include "$HOME/.config/jagen/env"
fi
jagen_try_include "$jagen_root/local.sh"

[ "$pkg_sdk" ] ||
    warning "pkg_sdk is not set"
[ "$pkg_dist_dir" ] ||
    error "pkg_dist_dir is not set"

export pkg_patch_dir="$pkg_dist_dir/patches"
export pkg_build_include_dir="$pkg_build_dir/include"
export pkg_private_dir="$pkg_src_dir/files"

in_flags ccache && use_env ccache
include "$pkg_lib_dir/env/cmake"
include "$pkg_lib_dir/env/sdk"

export CROSS_MAKE="make ARCH=${target_arch} CROSS_COMPILE=${target_system}-"
