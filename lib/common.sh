#!/bin/sh

message() {
    printf "(I) %s\n" "$*"
}

warning() {
    printf "(W) %s\n" "$*" >&2
}

error() {
    printf "(E) %s\n" "$*" >&2
}

debug() {
    if [ "$jagen_debug" ]; then
        printf "(D) %s\n" "$*"
    fi
}

die() {
    local ret=$?
    [ $ret = 0 ] && ret=1
    if [ $# = 0 ]; then
        error "The command exited with status: $ret"
    else
        error "$*"
    fi
    exit $ret
}

try_include() {
    if [ -f "$1" ]; then
        debug include "$1"
        . "$1"
    fi
}

include() {
    local pathname="${1:?}.sh"
    if [ -f "${pathname}" ]; then
        try_include "${pathname}"
    else
        return 2
    fi
}

import() {
    local pathname="${1:?}.sh"
    try_include "${jagen_dir:?}/lib/$pathname" || return
    for overlay in $jagen_overlays; do
        try_include "$jagen_dir/overlay/$overlay/$pathname" || return
    done
    if [ "$jagen_root" ]; then
        try_include "$jagen_root/$pathname" || return
    fi
}

use_env() {
    import "env/$1"
}

require() {
    import "require/$1"
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

real_path() {
    echo $(cd "$1"; pwd -P)
}

is_function() { type "$1" 2>/dev/null | grep -q 'function'; }

in_path() { $(which "$1" >/dev/null 2>&1); }

in_flags() { in_list "$1" $jagen_flags; }

add_PATH() {
    : ${1:?}
    PATH="$1":$(list_remove : "$1" $PATH)
}

add_LD_LIBRARY_PATH() {
    : ${1:?}
    LD_LIBRARY_PATH="$1":$(list_remove : "$1" $LD_LIBRARY_PATH)
}

_jagen() {
    ${jagen_lua:-lua} "$jagen_dir/src/main.lua" "$@"
}
