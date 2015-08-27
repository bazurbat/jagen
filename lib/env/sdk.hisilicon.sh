#!/bin/sh

use_env sdk android

target_system="arm-hisiv200-linux"
toolchain_bin_dir="${target_dir}/bin"

if [ ! "$BASH" ]; then
    error "Android build requires Bash shell, please run it and source the \
environment again."
    return 22
fi

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
