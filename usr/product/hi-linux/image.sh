#!/bin/sh

. "$jagen_lib_dir/main.sh" || exit

set -eu

mount_path=

calculate_size() {
    local du="$(du -sm "$1")"
    set -- $du
    echo $1
}

on_exit() {
    sudo umount "$mount_path"
    rmdir "$mount_path"
}

create_image() {
    local source_dir="$jagen_target_dir"
    local out_file="$jagen_build_dir/rootfs.ext4"
    local size="$(calculate_size "$source_dir")"

    # add some space for filesystem structures
    size=$((size + size/10))
    [ "$size" -gt 0 ] || die "Invalid image size, probably the $source_dir directory is empty"

    dd if=/dev/zero of="$out_file" bs=1M count=0 seek="$size" 2>/dev/null
    mke2fs -qF -t ext4 -O ^huge_file "$out_file" "${size}M"
 
    mount_path="$(mktemp -d)"

    sudo mount -t ext4 "$out_file" "$mount_path"
    trap on_exit EXIT
    sudo rsync -a --chown=0:0 "$source_dir/" "$mount_path"
}

create_image "$@"
