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
    if [ "$jagen_debug" ] && [ "$jagen_debug" -ge 0 ]; then
        printf "(D0) %s\n" "$*" >&2
    fi
}

debug1() {
    if [ "$jagen_debug" ] && [ "$jagen_debug" -ge 1 ]; then
        printf "(D1) %s\n" "$*" >&2
    fi
}

debug2() {
    if [ "$jagen_debug" ] && [ "$jagen_debug" -ge 2 ]; then
        printf "(D2) %s\n" "$*" >&2
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
        debug2 "include ${pathname}.sh"
        . "${pathname}.sh"
    elif [ -f "$pathname" ]; then
        debug2 "include $pathname"
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

include_from() {
    local name layer path
    name=${2-pkg/$(basename "${pkg__file:?}")}
    layer=$(jagen__find_layer "${1:?}")
    [ -d "$layer" ] || die "include_from $1: layer not found"
    path="$layer/$name"
    [ -f "$path" ] || die "include_from $1: $name not found in layer"
    debug1 "include_from $1: $name ($path)"
    . "$path"
}

try_require() {
    local filename
    debug2 "try_require $1"

    filename="$(find_in_path "${1:?}.sh")"
    if [ "$filename" ]; then
        debug2 "  using $filename"
        . "$filename"
    else
        return 2
    fi
}

require() {
    local IFS="$jagen_IFS"
    local name="${1:?}" path= i=
    debug2 "require $1"

    for i in $jagen_path; do
        path="$i/${name}.sh"
        if [ -f "$path" ]; then
            debug2 "  using $path"
            . "$path"
            return
        fi
    done

    die "require: could not find '$name' in import path"
}

import() {
    local IFS="$jagen_IFS"
    local name="${1:?}" path= i= found=
    debug2 "import $1"

    set -- $jagen_path
    i=$#

    while [ $i -gt 0 ]; do
        path="$(eval echo \$$i)/${name}.sh"
        if [ -f "$path" ]; then
            debug2 "  using $path"
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
    "$jagen_lua" "${jagen_dir:?}/src/Jagen.lua" "$@"
}

to_lower() {
    echo "${1:?}" | tr '[:upper:]' '[:lower:]'
}

jagen_name_to_id() {
    printf '%s' "${1:?}" | tr -c '[:alnum:]_' '_'
}

jagen__trim() {
    local var
    for var do
        [ "${var-}" ] && eval "$var=\${$var# }; $var=\${$var% }"
    done
}

# Evaluates the supplied value until there are no more expansions possible (no
# '$' symbols found in the value) and echoes the result of the expansion.
# Dies if the recursion depth exceeds 10.
jagen__expand() {
    local value="$1" name="$2" maxdepth=10 depth=0
    while [ $depth -le $maxdepth ]; do
        value=$(eval echo \""$value"\") || return
        if [ "$value" = "${value#*$}" ]; then
            echo "$value"; return
        fi
        depth=$((depth+1))
    done
    [ $depth -gt $maxdepth ] && die "the recursion depth exceeded $maxdepth "\
"while expanding${name:+ \$$name}: $value"
}

# Returns full pathname for the supplied layer specifier. Absolute paths are
# left intact, relative paths are resolved against the project directory and
# unqualified paths are tried against entries in jagen_layer_path.
jagen__find_layer() {
    local layer="${1:?}" path dir
    case $layer in
        /*) path="$layer" ;;
  ./*|../*) path="${jagen_project_dir:?}/$layer" ;;
         *) for dir in ${jagen_layer_path-} ${jagen_project_dir:?}; do
                path="$dir/$layer"
                [ -d "$path" ] && break
            done ;;
    esac
    printf "%s" "$(cd "$path" 2>&- && pwd -P)"
}

# Tries to expand the supplied arguments as layer paths relative to the current
# project directory and prints the jagen_FS-separated list of absolute
# directory paths. The '$jagen_dir/lib' is always added as the first entry of
# the result and '$jagen_project_lib_dir' as the last entry.
# Dies if it is unable to find a matching directory for any argument.
jagen__expand_layers() {
    local FS="$jagen_FS" layer dir path result="$jagen_dir/lib"
    for layer; do
        path=$(jagen__find_layer "$layer")
        if [ -z "$path" ]; then
            error "failed to find layer: $layer"
        else
            result="${result}${FS}${path}"
        fi
    done
    result="${result}${FS}${jagen_project_lib_dir}"
    printf '%s' "$result"
}

# Expands '$jagen_layers' and sets '$jagen_path' and '$LUA_PATH' accordingly.
jagen__set_path() {
    local layer path IFS="$jagen_IFS" FS="$jagen_FS"

    export jagen_path=
    export LUA_PATH="$jagen_dir/src/?.lua;;"

    for path in $(jagen__expand_layers ${jagen_layers-}); do
        jagen_path="${path}${FS}${jagen_path}"
        LUA_PATH="$path/?.lua;$LUA_PATH"
    done
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
    test -z "$(cd "${1:?}" 2>/dev/null && ls -A)"
}
