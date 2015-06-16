#!/bin/sh

build() {
    ninja -C "$pkg_build_dir" "$@"
}

rebuild() {
    echo rebuild
    echo ninja -C "$pkg_build_dir" "$@"
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
    *)
        echo "Unknown wrapper command: $1"
        exit 1
        ;;
esac
