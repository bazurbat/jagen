#!/bin/sh

message() { printf "\033[1;34m:::\033[0m %s\n" "$*"; }
warning() { printf "\033[1;33m:::\033[0m %s\n" "$*"; }
error() { printf "\033[1;31m:::\033[0m %s\n" "$*" >&2; }

die() {
    local ret=$?
    if [ $# = 0 ]; then
        error "The command exited with status: $ret"
    else
        error "$*"
    fi
    exit $ret
}

debug() {
    if [ "$pkg_debug" ]; then
        printf "\033[1;36m:::\033[0m %s\n" "$*"
    fi
    return 0
}

try_include() {
    if [ -f "$1" ]; then
        debug include "$1"
        . "$1"
        return $?
    fi
    return 1
}

include() {
    local name="${1:?}"
    local suffix="${2:-$pkg_sdk}"
    try_include "${name}.${suffix}.sh" || try_include "${name}.sh"
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

in_list() {
    local value="${1:?}"; shift
    for item; do
        [ "$item" = "$value" ] && return
    done
    return 1
}

list_remove() {
    local S="${1:?}" value="${2:?}"; shift 2
    local result
    local IFS="$S"
    set -- $@
    for item; do
        [ "$item" = "$value" ] || result="$result$S$item"
    done
    echo "${result#$S}"
}

is_function() { type "$1" 2>/dev/null | grep -q 'function'; }

in_path() { $(which "$1" >/dev/null 2>&1); }

in_flags() { in_list "$1" $pkg_flags; }

add_PATH() {
    : ${1:?}
    PATH="$1":$(list_remove : "$1" $PATH)
}

add_LD_LIBRARY_PATH() {
    : ${1:?}
    LD_LIBRARY_PATH="$1":$(list_remove : "$1" $LD_LIBRARY_PATH)
}

_jagen() {
    ${jagen_lua:-lua} "$pkg_lib_dir/jagen.lua" "$@"
}
