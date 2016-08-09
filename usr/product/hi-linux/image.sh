#!/bin/sh

. "$jagen_dir/env.sh" || exit

set -eu

mount_path=

calculate_size() {
    local du="$(du -sm "$1")"
    set -- $du
    echo $1
}

# add padding for filesystem structures
calculate_fs_size() {
    local size="${1:?}" divisor=8
    # integer ceil
    local padding=$(( size / divisor + (size % divisor != 0) ))
    echo $((size + padding))
}

on_exit() {
    sudo umount "$mount_path"
    rmdir "$mount_path"
}

create_image() {
    local source_dir="$jagen_build_dir/image"
    local out_file="$jagen_build_dir/rootfs.ext4"
    local size=$(calculate_size "$source_dir")
    local fs_size=$(calculate_fs_size $size)

    [ "$size" -gt 0 ] || die "Invalid image size, probably the $source_dir directory is empty"

    message "Creating $out_file (${size}M/${fs_size}M) from $source_dir"

    dd if=/dev/zero of="$out_file" bs=1M count=0 seek="$fs_size" 2>/dev/null
    mke2fs -qF -t ext4 -O ^huge_file "$out_file" "${fs_size}M"

    mount_path="$(mktemp -d)"

    sudo mount -t ext4 "$out_file" "$mount_path"
    trap on_exit EXIT
    sudo rsync -a --chown=0:0 "$source_dir/" "$mount_path"
}

create_image "$@"
