#!/bin/sh

toolchain_bin=$(dirname $(command -v arm-hisiv200-linux-gnueabi-gcc 2>/dev/null))

generate_toolchain_wrappers() {
    local name

    [ -d "$target_bin_dir" ] && rm -r "$target_bin_dir"
    mkdir -p "$target_bin_dir"

    for bin in "$toolchain_bin"/*; do
        name=$(basename $bin | cut -d- -f5-)
        ln -s "$bin" "$target_bin_dir/${target_system}-${name}"
    done
}
