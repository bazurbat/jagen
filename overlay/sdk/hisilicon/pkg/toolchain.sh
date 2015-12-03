#!/bin/sh

jagen_pkg_install() {
    : ${jagen_toolchain_dir:?}
    : ${jagen_target_toolchain_dir:?}

    local bin name

    rm -fr "$jagen_target_toolchain_dir"
    mkdir -p "$jagen_target_toolchain_dir"

    for bin in "$jagen_toolchain_dir"/bin/*; do
        name="$(basename "$bin" | cut -d- -f5-)"
        ln -sf "$bin" "${jagen_target_toolchain_dir}/${jagen_target_system}-${name}"
    done
}
