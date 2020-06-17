#!/bin/sh

jagen__last_error=0

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
    jagen__last_error=$ret
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
    else
        error "include $pathname not found"
        return 2
    fi
}

find_in_path() {
    local IFS="$jagen_IFS"
    set -- "$@"
    local layer name path

    for layer in $jagen_path; do
        for name; do
            path="$layer/$name"
            if [ -f "$path" ]; then
                printf '%s' "$path"
                return
            fi
        done
    done
}

include_from() {
    local name layer path
    name=${2-pkg/$(basename "${pkg__file:?}")}
    layer=$(jagen__resolve_layer "${1:?}")
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
        if [ "$path" != "$jagen_root_dir/env.sh" -a -f "$path" ]; then
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
    if [ -t 1 ]; then
        export jagen__has_console=1
    fi
    LUA_PATH="$jagen_dir/src/?.lua" \
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

# Returns 0 if the first argument is the subdirectory of the second
jagen_is_subdir() {
    local dir1="$1" dir2="$2"
    dir1="$([ "$dir1" ] && cd "$dir1" 2>&- && pwd -P)"
    dir2="$([ "$dir2" ] && cd "$dir2" 2>&- && pwd -P)"
    test "$dir1" -a "$dir2" && test "$dir2" != "${dir2#${dir1}}"
}

# Evaluates the supplied value until there are no more expansions possible and
# echoes the result of the expansion.
# Dies if the recursion depth exceeds 10.
jagen__expand() {
    local value="$1" name="$2" maxdepth=10 depth=0
    while [ $depth -le $maxdepth ]; do
        if ! echo "$value" | grep -lq -e '${' -e '$[[:alpha:]]'; then
            if [ "$value" != "${value#*\`}" -o "$value" != "${value#*\$(}" ]; then
                value=$(eval echo \""$value"\")
            fi
            echo "$value"; return
        fi
        # escape command substitution for the next eval
        value=$(echo "$value" | sed -e 's/$(/\\$(/g' -e 's/`/\\`/g')
        # double quoting here is necessary to preserve alternative IFS
        # characters (such as newline) inside the value
        value=$(eval echo \""$value"\") || return
        depth=$((depth+1))
    done
    [ $depth -gt $maxdepth ] && die "the recursion depth exceeded $maxdepth "\
"while expanding${name:+ \$$name}: $value"
}

# Returns full pathname for the supplied layer specifier. Absolute paths are
# left intact, relative paths are resolved against the project directory and
# unqualified paths are tried against entries in jagen_layer_path.
jagen__resolve_layer() {
    local layer="${1:?}" path dir
    case $layer in
        /*) path="$layer" ;;
  ./*|../*) path="${jagen_root_dir:?}/$layer" ;;
         *) for dir in ${jagen_layer_path-} .; do
                case $dir in
                    /*) ;;
                     *) dir="$jagen_root_dir/$dir" ;;
                esac
                path="$dir/$layer"
                [ -d "$path" ] && break
            done ;;
    esac
    cd "$path" 2>&- && pwd -P
    return 0
}

# Tries to resolve all entries in $jagen_layers and prints the
# jagen_S-separated list of absolute directory paths.
jagen__resolve_layers() {
    local layer path result= rv=0 IFS="$jagen_S"
    for layer in ${jagen_layers-}; do
        path=$(jagen__resolve_layer "$layer")
        if [ -z "$path" ]; then
            error "failed to resolve layer: $layer"
            rv=2
        else
            result="${result}${jagen_S}${path}"
        fi
    done
    echo "${result#$jagen_S}"
    return $rv
}

jagen__get_path() {
    local path rv=0
    path="$jagen_dir/lib"
    path="${path}${jagen_S}$(jagen__resolve_layers)"; rv=$?
    path="${path}${jagen_S}$jagen_root_lib_dir"
    echo "$path"
    return $rv
}

# Sets '$jagen_path' for the current project.
jagen__set_path() {
    local item paths IFS="$jagen_S"

    paths=$(jagen__get_path) || return

    export jagen_path=

    for item in $paths; do
        jagen_path="${item}${jagen_FS}${jagen_path}"
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
    if [ -z "$jagen__cached_cmake_version" ]; then
        jagen__cached_cmake_version=$(cmake --version | head -1 | cut -d' ' -f3)
    fi
    printf "%s" "$jagen__cached_cmake_version"
}

jagen__is_empty() {
    test -z "$(cd "${1:?}" 2>/dev/null && ls -A)"
}

jagen__need_cmd() {
    if ! command -v "$1" > /dev/null 2>&1; then
        die "need '$1': command not found"
    fi
}

jagen__get_bitness() {
    jagen__need_cmd head
    # Architecture detection without dependencies beyond coreutils.
    # ELF files start out "\x7fELF", and the following byte is
    #   0x01 for 32-bit and
    #   0x02 for 64-bit.
    # The printf builtin on some shells like dash only supports octal
    # escape sequences, so we use those.
    local _current_exe_head=$(head -c 5 /proc/self/exe )
    if [ "$_current_exe_head" = "$(printf '\177ELF\001')" ]; then
        printf "%s" 32
    elif [ "$_current_exe_head" = "$(printf '\177ELF\002')" ]; then
        printf "%s" 64
    else
        die "unknown platform bitness"
    fi
}

jagen__get_endianness() {
    local cputype=$1
    local suffix_eb=$2
    local suffix_el=$3

    # detect endianness without od/hexdump, like jagen__get_bitness() does.
    jagen__need_cmd head
    jagen__need_cmd tail

    local _current_exe_endianness="$(head -c 6 /proc/self/exe | tail -c 1)"
    if [ "$_current_exe_endianness" = "$(printf '\001')" ]; then
        printf "%s" "${cputype}${suffix_el}"
    elif [ "$_current_exe_endianness" = "$(printf '\002')" ]; then
        printf "%s" "${cputype}${suffix_eb}"
    else
        die "unknown platform endianness"
    fi
}

jagen_get_system() {
    local _ostype="$(uname -s)"
    local _cputype="$(uname -m)"

    if [ "$_ostype" = Linux ]; then
        if [ "$(uname -o)" = Android ]; then
            local _ostype=Android
        fi
    fi

    if [ "$_ostype" = Darwin -a "$_cputype" = i386 ]; then
        # Darwin `uname -s` lies
        if sysctl hw.optional.x86_64 | grep -q ': 1'; then
            local _cputype=x86_64
        fi
    fi

    case "$_ostype" in

        Android)
            local _ostype=linux-android
            ;;

        Linux)
            local _ostype=unknown-linux-gnu
            ;;

        FreeBSD)
            local _ostype=unknown-freebsd
            ;;

        NetBSD)
            local _ostype=unknown-netbsd
            ;;

        DragonFly)
            local _ostype=unknown-dragonfly
            ;;

        Darwin)
            local _ostype=apple-darwin
            ;;

        MINGW* | MSYS* | CYGWIN*)
            local _ostype=pc-windows-gnu
            ;;

        *)
            die "unrecognized OS type: $_ostype"
            ;;

    esac

    case "$_cputype" in

        i386 | i486 | i686 | i786 | x86)
            local _cputype=i686
            ;;

        xscale | arm)
            local _cputype=arm
            if [ "$_ostype" = "linux-android" ]; then
                local _ostype=linux-androideabi
            fi
            ;;

        armv6l)
            local _cputype=arm
            if [ "$_ostype" = "linux-android" ]; then
                local _ostype=linux-androideabi
            else
                local _ostype="${_ostype}eabihf"
            fi
            ;;

        armv7l | armv8l)
            local _cputype=armv7
            if [ "$_ostype" = "linux-android" ]; then
                local _ostype=linux-androideabi
            else
                local _ostype="${_ostype}eabihf"
            fi
            ;;

        aarch64)
            local _cputype=aarch64
            ;;

        x86_64 | x86-64 | x64 | amd64)
            local _cputype=x86_64
            ;;

        mips)
            local _cputype="$(jagen__get_endianness $_cputype "" 'el')"
            ;;

        mips64)
            local _bitness="$(jagen__get_bitness)"
            if [ $_bitness = "32" ]; then
                if [ $_ostype = "unknown-linux-gnu" ]; then
                    # 64-bit kernel with 32-bit userland
                    # endianness suffix is appended later
                    local _cputype=mips
                fi
            else
                # only n64 ABI is supported for now
                local _ostype="${_ostype}abi64"
            fi

            local _cputype="$(jagen__get_endianness $_cputype "" 'el')"
            ;;

        ppc)
            local _cputype=powerpc
            ;;

        ppc64)
            local _cputype=powerpc64
            ;;

        ppc64le)
            local _cputype=powerpc64le
            ;;

        *)
            die "unknown CPU type: $_cputype"

    esac

    # Detect 64-bit linux with 32-bit userland
    if [ $_ostype = unknown-linux-gnu -a $_cputype = x86_64 ]; then
        if [ "$(jagen__get_bitness)" = "32" ]; then
            local _cputype=i686
        fi
    fi

    # Detect armv7 but without the CPU features Rust needs in that build,
    # and fall back to arm.
    # See https://github.com/rust-lang-nursery/rustup.rs/issues/587.
    if [ $_ostype = "unknown-linux-gnueabihf" -a $_cputype = armv7 ]; then
        if ensure grep '^Features' /proc/cpuinfo | grep -q -v neon; then
            # At least one processor does not have NEON.
            local _cputype=arm
        fi
    fi

    printf "%s" "$_cputype-$_ostype"
}

case $(uname) in
    Darwin)
        jagen_esed() { sed -E "$@"; }
        jagen_get_file_size() {
            local size=
            size=$(stat -f'%z' "${1:?}" 2>/dev/null)
            [ $? = 0 ] && echo "$size" || echo 0
        }
        ;;
    *)
        jagen_esed() { sed -r "$@"; }
        jagen_get_file_size() {
            local size=
            size=$(stat -c'%s' "${1:?}" 2>/dev/null)
            [ $? = 0 ] && echo "$size" || echo 0
        }
        ;;
esac

jagen_is_same_dir() {
    local dir1="$1" dir2="$2"
    [ "$dir1" = "$dir2" ] && return
    dir1=$([ "$dir1" ] && cd "$dir1" 2>&- && pwd -P)
    dir2=$([ "$dir2" ] && cd "$dir2" 2>&- && pwd -P)
    test "$dir1" -a "$dir2" -a "$dir1" = "$dir2"
}

jagen_get_file_checksum() {
    local prog="${1:?}sum" file="${2:?}" out=
    if [ -z "$(command -v "$prog")" ]; then
        warning "$prog is not found in PATH, can not calculate a checksum for $file"
        return 2
    fi
    out=$("$prog" "$file")
    [ $? = 0 ] && echo "$out" | awk '{print $1}'
}

jagen_system_is_wsl() {
    grep -q Microsoft /proc/version 2>&-
}
