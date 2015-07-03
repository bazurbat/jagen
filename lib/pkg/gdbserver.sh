#!/bin/sh

use_toolchain target

workdir="gdb/gdbserver"

pkg_build() {
    p_run cd "$workdir"
    p_run ./configure \
        --host="$target_system" \
        --prefix="" \
        --program-transform-name='' \
        --disable-werror

    p_run make
}

pkg_install() {
    p_run cd "$workdir"
    p_run make DESTDIR="$sdk_rootfs_prefix" install
}
