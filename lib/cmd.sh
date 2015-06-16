#!/bin/sh

. "$pkg_lib_dir/pkg.sh" || exit 1

build() {
    echo build
    echo ninja -C "$pkg_build_dir" "$@"
}

rebuild() {
    echo rebuild
    echo ninja -C "$pkg_build_dir" "$@"
}

status() {
    local name="$1"
    local location="$2"
    local dir="$3"
    local repo="$pkg_src_dir/${dir:-${location:+$(basename "$location" .git)}}"
    local head=$(p_src_head "$repo")
    local dirty=$(p_src_is_dirty "$repo")

    echo "$name: $head $dirty"
}

case $1 in
    build)
        shift
        build "$@"
        ;;
    rebuild)
        shift
        rebuild "$@"
        ;;
    status)
        shift
        status "$@"
        ;;
    *)
        echo "Unknown wrapper command: $1"
        exit 1
        ;;
esac
