#!/bin/sh

use_toolchain target

workdir="gdb/gdbserver"

jagen_pkg_build() {
    pkg_run cd "$workdir"
    pkg_run ./configure \
        --host="$target_system" \
        --prefix="" \
        --program-transform-name='' \
        --disable-werror

    pkg_run make
}

jagen_pkg_install() {
    pkg_run cd "$workdir"
    pkg_run make DESTDIR="$sdk_rootfs_prefix" install
}
