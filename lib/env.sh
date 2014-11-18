#!/bin/sh

if [ -z "$ja_root" ]; then
    export ja_root="."
fi

export ja_bin_dir="$ja_root/bin"
export ja_lib_dir="$ja_root/lib"
export ja_src_dir="$ja_root/src"

export ja_sdk="sigma"

export pkg_build_type="Release"
export pkg_build_dir="$ja_root/build"
export pkg_dist_dir="$ja_root/dist/$ja_sdk"

export ja_bin="chibi-scheme -r $ja_lib_dir/jagen.scm"

debug() {
    if [ "$ja_debug" = "yes" ]; then
        printf "\033[1;36m:::\033[0m %s\n" "$*"
    fi
    return 0
}

message() { printf "\033[1;34m:::\033[0m %s\n" "$*"; }
warning() { printf "\033[1;33m:::\033[0m %s\n" "$*"; }
error() { printf "\033[1;31m:::\033[0m %s\n" "$*" >&2; }

die() { error "$*"; exit 1; }

include() {
    local basepath="$1"

    if [ -f "${basepath}.${ja_sdk}.sh" ]; then
        debug include ${basepath}.${ja_sdk}.sh
        . "${basepath}.${ja_sdk}.sh"
    elif [ -f "${basepath}.sh" ]; then
        debug include ${basepath}.sh
        . "${basepath}.sh"
    else
        debug include not found $basepath
    fi
}

use_env() {
    local e
    for e in "$@"; do
        include "$ja_lib_dir/env/$e"
    done
}

include "${HOME}/.config/jagen/env"
include "$ja_root/local"
include "$ja_lib_dir/env/cmake"
include "$ja_lib_dir/env/sdk"

pkg_build_dir=$(realpath "$pkg_build_dir")
pkg_dist_dir=$(realpath "$pkg_dist_dir")

: ${ja_files_dir:="$ja_src_dir/files"}
export ja_files_dir
