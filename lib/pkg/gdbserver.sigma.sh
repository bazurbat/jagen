#!/bin/sh

p_source="$p_dist_dir/gdb-7.6.2.tar.bz2"

use_env target

workdir="gdb/gdbserver"

pkg_build() {
    cd "$workdir" || return $?
    p_run ./configure \
        --host="mipsel-linux" \
        --prefix=""

    p_run make
}

pkg_install() {
    cd "$workdir" || return $?
    p_run make DESTDIR="$sdk_rootfs_prefix" install
}
