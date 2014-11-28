#!/bin/sh

message() { printf "\033[1;34m:::\033[0m %s\n" "$*"; }
warning() { printf "\033[1;33m:::\033[0m %s\n" "$*"; }
error() { printf "\033[1;31m:::\033[0m %s\n" "$*" >&2; }

die() {
    local status=$?
    if [ $# = 0 ]; then
        error "The command exited with status: $status"
    else
        error "$*"
    fi
    exit $status
}

debug() {
    if [ "$pkg_debug" = "yes" ]; then
        printf "\033[1;36m:::\033[0m %s\n" "$*"
    fi
    return 0
}

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

use_toolchain() {
    local f
    for f in "$@"; do
        include "$pkg_lib_dir/toolchain/$f"
    done
}
