#!/bin/sh

. "$jagen_dir/env.sh" || exit

set -eu

mount_path=
image_file=
out_dir="$jagen_project_dir/../out"

calculate_size() {
    local du="$(du -sm "$1")"
    set -- $du
    echo $1
}

# add padding for filesystem structures
calculate_fs_size() {
    local size="${1:?}" divisor=6
    # integer ceil
    local padding=$(( size / divisor + (size % divisor != 0) ))
    echo $((size + padding))
}

on_exit() {
    sudo umount "$mount_path"
    rmdir "$mount_path"
}

create_image() {
    local target_board="${1:?target board is not specified}"
    local version="${2:?version is not specified}"
    local source_dir="$jagen_build_dir/image"
    local size=$(calculate_size "$source_dir")
    local fs_size=$(calculate_fs_size $size)
    local name="$(echo $target_board | tr '[:lower:]' '[:upper:]')"

    image_file="$jagen_build_dir/${name}_${version}.ext4"

    [ "$size" -gt 0 ] || die "Invalid image size, probably the $source_dir directory is empty"

    message "Creating $image_file (${size}M/${fs_size}M) from $source_dir"

    dd if=/dev/zero of="$image_file" bs=1M count=0 seek="$fs_size" 2>/dev/null
    mke2fs -qF -t ext4 -O ^huge_file "$image_file" "${fs_size}M"

    mount_path="$(mktemp -d)"

    sudo mount -t ext4 "$image_file" "$mount_path"
    trap on_exit EXIT
    sudo rsync -a --chown=0:0 "$source_dir/" "$mount_path"
}

move_to_output() {
    mkdir -p "$out_dir"
    mv -f "$image_file" "$out_dir"

    message "New firmware image is ready:"
    ls -t1 "$out_dir"/*.ext4 | head -n1
}

create_image "$@"
move_to_output

if in_flags notify_slack && [ "$(command -v jagen-notify-slack)" ]; then
    jagen-notify-slack new_image "$(cd "$out_dir" && ls -t1 *.ext4 | head -n1)"
fi
