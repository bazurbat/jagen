#!/bin/sh

use_env target

workdir="gdb/gdbserver"

jagen_pkg_compile() {
    pkg_run cd "$workdir"
    pkg_run "$pkg_source_dir/configure" \
        --host="$jagen_target_system" \
        --prefix="" \
        --program-transform-name='' \
        --disable-werror

    pkg_run make
}

jagen_pkg_install() {
    pkg_run cd "$workdir"
    pkg_run make DESTDIR="$jagen_sdk_rootfs_prefix" install
}
