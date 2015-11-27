#!/bin/sh

jagen_pkg_install() {
    : ${jagen_toolchain_dir:?}
    : ${toolchain_bin_dir:?}
    : ${target_system:?}

    local bin name

    rm -fr "$toolchain_bin_dir"
    mkdir -p "$toolchain_bin_dir"

    for bin in "$jagen_toolchain_dir"/bin/*; do
        name="$(basename "$bin" | cut -d- -f5-)"
        ln -rsf "$bin" "${toolchain_bin_dir}/${target_system}-${name}"
    done
}
