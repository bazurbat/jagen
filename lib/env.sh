#!/bin/sh

debug() {
    if [ "$pkg_debug" = "yes" ]; then
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

    if [ -f "${basepath}.${pkg_sdk}.sh" ]; then
        debug include ${basepath}.${pkg_sdk}.sh
        . "${basepath}.${pkg_sdk}.sh"
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
        include "$pkg_lib_dir/env/$e"
    done
}

include "${HOME}/.config/jagen/env"
include "$pkg_root/local"
include "$pkg_lib_dir/env/cmake"
include "$pkg_lib_dir/env/sdk"

: ${pkg_private_dir:="$pkg_src_dir/files"}
: ${pkg_dist_dir:="$pkg_root/dist/$pkg_sdk"}
export pkg_private_dir pkg_dist_dir
