#!/bin/sh

p_source="$pkg_dist_dir/loop-AES-v3.7b.tar.bz2"

use_toolchain target

export ARCH=mips

pkg_build() {
    p_run make \
        LINUX_SOURCE="$LINUX_KERNEL" \
        KBUILD_OUTPUT="$LINUX_KERNEL" \
        USE_KBUILD=y MODINST=n RUNDM=n
}

pkg_install() {
    cd "tmp-d-kbuild" &&
    p_install_modules "loop"
}
