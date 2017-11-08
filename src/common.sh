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

include() {
    local pathname="${1:?}"
    if [ -f "${pathname}.sh" ]; then
        debug "include ${pathname}.sh"
        . "${pathname}.sh"
    elif [ -f "$pathname" ]; then
        debug "include $pathname"
        . "$pathname"
    fi
}

find_in_path() {
    local IFS="$jagen_IFS"
    local name="${1:?}" path= i=

    for i in $jagen_path; do
        path="$i/$name"
        if [ -f "$path" ]; then
            printf "$path"
            return
        fi
    done
}

try_require() {
    local filename
    debug "try_require $1"

    filename="$(find_in_path "${1:?}.sh")"
    if [ "$filename" ]; then
        debug "  using $filename"
        . "$filename"
    else
        return 2
    fi
}

require() {
    local IFS="$jagen_IFS"
    local name="${1:?}" path= i=
    debug "require $1"

    for i in $jagen_path; do
        path="$i/${name}.sh"
        if [ -f "$path" ]; then
            debug "  using $path"
            . "$path"
            return
        fi
    done

    die "require: could not find '$name' in import path"
}

import() {
    local IFS="$jagen_IFS"
    local name="${1:?}" path= i= found=
    debug "import $1"

    set -- $jagen_path
    i=$#

    while [ $i -gt 0 ]; do
        path="$(eval echo \$$i)/${name}.sh"
        if [ -f "$path" ]; then
            debug "  using $path"
            . "$path" ||
                die "import $name: error while sourcing '$path'"
            found=1
        fi
        i=$((i-1))
    done

    [ "$found" ] && return 0 || return 2
}

use_env() {
    import "env/${1:?}"
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
    local result=
    local IFS="$S"
    set -- $@
    for item; do
        [ "$item" = "$value" ] || result="$result$S$item"
    done
    echo "${result#$S}"
}

real_path() {
    (cd "$1" >/dev/null 2>&1 && pwd -P)
}

is_function() {
    local out="$(type "$1" 2>/dev/null)"
    case $out in
        *function*) return 0 ;;
                 *) return 1 ;;
    esac
}

in_path() { $(which "$1" >/dev/null 2>&1); }

in_flags() {
    local IFS; unset IFS
    in_list "$1" $jagen_flags
}

add_PATH() {
    : ${1:?}
    PATH="$1":$(list_remove : "$1" $PATH)
    PATH="${PATH%:}"
}

add_LD_LIBRARY_PATH() {
    : ${1:?}
    LD_LIBRARY_PATH="$1":$(list_remove : "$1" ${LD_LIBRARY_PATH-})
    LD_LIBRARY_PATH="${LD_LIBRARY_PATH%:}"
}

_jagen() {
    [ "$jagen_lua" ] || die "Lua >=5.1 or compatible (LuaJIT 2.0) is required to run Jagen but it was not found."
    "$jagen_lua" "$jagen_dir/src/Jagen.lua" "$@"
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

jagen__versions() {
    _jagen _compare_versions "$@"
}

jagen__get_cmake_version() {
    printf "%s" $(cmake --version | head -1 | cut -d' ' -f3)
}

jagen__is_empty() {
    test "$(cd "${1:?}" 2>/dev/null && echo *)" = '*'
}
