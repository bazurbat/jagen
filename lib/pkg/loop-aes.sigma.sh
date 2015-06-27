#!/bin/sh

use_toolchain target

export ARCH=mips

pkg_build() {
    PATH="$jagen_toolchain_dir/bin:$PATH"

    p_run make \
        LINUX_SOURCE="$LINUX_KERNEL" \
        KBUILD_OUTPUT="$LINUX_KERNEL" \
        USE_KBUILD=y MODINST=n RUNDM=n
}

pkg_install() {
    cd "tmp-d-kbuild" &&
    p_install_modules "loop"
}
