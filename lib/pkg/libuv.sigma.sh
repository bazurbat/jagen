#!/bin/sh

psource="libuv-v0.10.25"

use_env target

pkg_prepare() {
    if [ -x ./autogen.sh ]; then
        p_cmd ./autogen.sh
    fi
}

pkg_build() {
    if [ -x ./configure ]; then
        p_cmd ./configure \
            --host="mipsel-linux" \
            --prefix="" \
            --with-sysroot="$rootfs_cross_root"
    fi

    p_make
}

pkg_install() {
    if [ -x ./configure ]; then
        p_make DESTDIR="$rootfs_cross_root" install
    else
        p_cmd cp -vaf include "$rootfs_cross_root"
        # cp -vaf libuv.a "${rootfs_cross_root}/lib" \
        #     >>"$plog" 2>&1 || return $?
        p_cmd cp -vaf libuv.so "${rootfs_cross_root}/lib/libuv.so.0.10.so"
        p_cmd cd "${rootfs_cross_root}/lib"
        p_cmd ln -sf libuv.so.0.10.so libuv.so.0.10
        p_cmd ln -sf libuv.so.0.10.so libuv.so
    fi
}
