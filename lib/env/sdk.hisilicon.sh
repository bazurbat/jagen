#!/bin/sh

use_env sdk android

target_system="arm-hisiv200-linux"

make_toolchain() {
    local bin name

    if ! [ "$jagen_toolchain_dir" ]; then
        error "jagen_toolchain_dir is not set"
        return 1
    fi

    [ -d "$toolchain_bin_dir" ] && rm -r "$toolchain_bin_dir"
    mkdir -p "$toolchain_bin_dir"

    for bin in "$jagen_toolchain_dir"/bin/*; do
        name="$(basename "$bin" | cut -d- -f5-)"
        ln -sf "$bin" "${toolchain_bin_dir}/${target_system}-${name}"
    done
}
