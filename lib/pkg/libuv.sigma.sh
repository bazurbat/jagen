#!/bin/sh

p_source="$pkg_dist_dir/libuv-v0.10.25.tar.gz"

use_toolchain target

pkg_patch() {
    if [ -x ./autogen.sh ]; then
        p_run ./autogen.sh
    fi
}

pkg_build() {
    if [ -x ./configure ]; then
        p_run ./configure \
            --host="mipsel-linux" \
            --prefix="" \
            --with-sysroot="$sdk_rootfs_prefix"
    fi

    p_run make
}

pkg_install() {
    if [ -x ./configure ]; then
        p_run make DESTDIR="$sdk_rootfs_prefix" install
    else
        p_run cp -vaf include "$sdk_rootfs_prefix"
        # cp -vaf libuv.a "${sdk_rootfs_prefix}/lib" \
        #     >>"$p_log" 2>&1 || return $?
        p_run cp -vaf libuv.so "${sdk_rootfs_prefix}/lib/libuv.so.0.10.so"
        p_run cd "${sdk_rootfs_prefix}/lib"
        p_run ln -sf libuv.so.0.10.so libuv.so.0.10
        p_run ln -sf libuv.so.0.10.so libuv.so
    fi
}
