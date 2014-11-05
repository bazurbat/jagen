#!/bin/sh

export ja_bin_dir="$ja_root/bin"
export ja_lib_dir="$ja_root/lib"
export ja_src_dir="$ja_root/src"

export ja_build_dir="$ja_root/build"
export ja_build_type="Release"

export ja_bin="chibi-scheme -r $ja_lib_dir/jagen.scm"
export ja_sdk="sigma"

debug() { [ "$ja_debug" ] && printf "\033[1;36m:::\033[0m %s\n" "$*"; }

message() { printf "\033[1;34m:::\033[0m %s\n" "$*"; }

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

if [ -z "$ja_root" ]; then
    die "base directory is not set"
fi

include "$ja_root/env"
