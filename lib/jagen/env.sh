#!/bin/sh

if [ -z "$ja_basedir" ]; then
    export ja_basedir="$(realpath $(dirname $0))"
fi

export ja_bindir="$ja_basedir/bin"
export ja_libdir="$ja_basedir/lib"
export ja_srcdir="$ja_basedir/src"

export ja_builddir="$ja_basedir/build"

export ja_bin="chibi-scheme -r $ja_libdir/jagen/jagen.scm"

export ja_sdk=""

local_env="$ja_basedir/env.sh"
[ -f "$local_env" ] || touch "$local_env"
. "$local_env"

message() { printf "\033[1;34m:::\033[0m %s\n" "$@"; }

error() { printf "\033[1;31m:::\033[0m %s\n" "$@" >&2; }

die() { error "$@"; exit 1; }

use_env() {
    local e
    for e in "$@"; do
        local f="$ja_libdir/env/${e}.sh"
        [ -f "$f" ] && . "$f"
    done
}
