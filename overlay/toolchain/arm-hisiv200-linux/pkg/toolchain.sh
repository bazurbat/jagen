#!/bin/sh

jagen_pkg_install() {
    : ${jagen_toolchain_bin_dir:?}
    : ${jagen_toolchain_dir:?}
    : ${jagen_toolchain_prefix:?}

    local bin name

    rm -fr "$jagen_toolchain_bin_dir"
    mkdir -p "$jagen_toolchain_bin_dir"

    for bin in "$jagen_toolchain_dir"/bin/*; do
        name="$(basename "$bin" | cut -d- -f5-)"
        ln -fsr "$bin" "${jagen_toolchain_prefix}${name}"
    done
}
