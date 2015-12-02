#!/bin/sh

jagen_pkg_install() {
    : ${jagen_toolchain_dir:?}
    : ${toolchain_dir:?}

    local bin name

    rm -fr "$toolchain_dir/bin"
    mkdir -p "$toolchain_dir/bin"

    for bin in "$jagen_toolchain_dir"/bin/*; do
        name="$(basename "$bin" | cut -d- -f5-)"
        ln -sf "$bin" "${toolchain_dir}/bin/${target_system}-${name}"
    done
}
