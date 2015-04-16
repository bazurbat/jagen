#!/bin/sh

generate_toolchain_wrappers() {
    local bin name
    local gcc_file gcc_dir
    local gcc_name=arm-hisiv200-linux-gnueabi-gcc

    gcc_file="$(command -v "$gcc_name" 2>/dev/null)"
    [ "$gcc_file" ] ||
        die "Toolchain executable '$gcc_name' was not found in path"
    gcc_dir="$(dirname "$gcc_file")"

    [ -d "$target_bin_dir" ] && rm -r "$target_bin_dir"
    mkdir -p "$target_bin_dir"

    for bin in "$gcc_dir"/*; do
        name="$(basename "$bin" | cut -d- -f5-)"
        ln -s "$bin" "$target_bin_dir/${target_system}-${name}"
    done
}
