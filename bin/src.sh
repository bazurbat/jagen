#!/bin/sh

if [ -z "$ja_root" ]; then
    export ja_root=$(realpath $(dirname $0)/..)
fi

. "$ja_root/lib/pkg.sh" || die "Failed to load pkg env"

cmd="$1"
src="$2"
dst="$3"

[ "$cmd" ] || die "No command specified"
[ -d "$src" ] || die "The source directory '$src' is not exists"

sources=""
sources="$sources chicken-eggs"
sources="$sources chicken-scheme"
sources="$sources files"
sources="$sources karaoke-player"
sources="$sources linux"
sources="$sources sigma-ezboot"
sources="$sources sigma-kernel"
sources="$sources sigma-mrua"
sources="$sources sigma-rootfs"
sources="$sources sigma-utils"
sources="$sources syslink"
sources="$sources u-boot"

case $cmd in
    pull|reset|clean)
        for dir in $sources; do
            message "$cmd $dir"
            p_src_${cmd} "$src/$dir"
        done
        message "${cmd} astindex"
        p_src_${cmd} "$src/karaoke-player/source/astindex"
        ;;
    upload)
        for dir in $sources; do
            message "upload $dir"
            p_src_upload "$src/$dir" "$dst"
        done
        message "upload astindex"
        p_src_upload "$src/karaoke-player/source/astindex" "$dst"
        ;;
    download)
        for dir in $sources; do
            message "download $dir"
            p_src_download "$src/${dir}.tar" "$dst"
        done
        message "download astindex"
        p_src_download "$src/astindex.tar" "$dst/karaoke-player/source"
        ;;
    *)
        die "Unknown command: $cmd"
        ;;
esac
