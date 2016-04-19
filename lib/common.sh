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
        return 2 # stands for ENOENT
    fi
}

jagen_find_path() {
    local path="${1:?}" result= i=
    : ${jagen_dir:?}

    if [ "$jagen_root" ]; then
        result="$jagen_root/$path"
        if [ -e "$result" ]; then
            echo "$result"
            return
        fi
    fi

    set -- $jagen_overlays
    i=$#

    while [ "$i" -gt 0 ]; do
        result="$jagen_dir/overlay/$(eval echo \$$i)/$path"
        if [ -e "$result" ]; then
            echo "$result"
            return
        fi
        i=$((i-1))
    done

    result="$jagen_dir/lib/$path"
    if [ -e "$result" ]; then
        echo "$result"
    fi
}

import() {
    local name="${1:?}" sts=
    import "${jagen_dir:?}/usr/${jagen_product:?}/${name}"
    sts=$?; [ $sts != 2 ] && return $sts
    include "${jagen_dir:?}/vendor/${name}"
    sts=$?; [ $sts != 2 ] && return $sts
    include "${jagen_dir:?}/lib/${name}"
    sts=$?; [ $sts != 2 ] && return $sts
}

use_env() {
    import "env/${1:?}"
}

require() {
    import "require/${1:?}"
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

to_lower() {
    echo "${1:?}" | tr '[:upper:]' '[:lower:]'
}

jagen_nproc() {
    case $(uname -s) in
        Darwin)
            sysctl -n hw.ncpu ;;
        *)
            nproc
    esac
}
