#!/bin/sh

use_env sdk android

target_system="arm-hisiv200-linux"
target_product="Hi3719CV100"

toolchain_bin_dir="${jagen_target_dir}/bin"

make_toolchain() {
    : ${jagen_toolchain_dir:?}

    local bin name

    rm -fr "$toolchain_bin_dir"
    mkdir -p "$toolchain_bin_dir"

    for bin in "$jagen_toolchain_dir"/bin/*; do
        name="$(basename "$bin" | cut -d- -f5-)"
        ln -sf "$bin" "${toolchain_bin_dir}/${target_system}-${name}"
    done
}
