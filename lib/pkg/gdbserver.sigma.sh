#!/bin/sh

psource="gdb-7.6.2"

use_env target

workdir="gdb/gdbserver"

pkg_build() {
    cd "$workdir" || return $?
    p_cmd ./configure \
        --host="mipsel-linux" \
        --prefix=""

    p_make
}

pkg_install() {
    cd "$workdir" || return $?
    p_make DESTDIR="$rootfs_cross_root" install
}
