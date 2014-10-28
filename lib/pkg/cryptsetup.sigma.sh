#!/bin/sh

psource="cryptsetup-1.6.2"

use_env target

pkg_prepare() {
    p_cmd autoreconf -vif
}

pkg_build() {
    p_cmd ./configure \
        --host="mipsel-linux" \
        --prefix="" \
        --disable-dependency-tracking \
        --disable-static \
        --enable-shared \
        --disable-nls \
        --disable-rpath \
        --disable-pwquality \
        --disable-veritysetup \
        --disable-selinux \
        --enable-udev \
        --disable-kernel_crypto \
        --disable-python \
        --with-sysroot="$rootfs_cross_root" \
        --with-crypto_backend=gcrypt \
        --with-libgcrypt-prefix="$rootfs_cross_root"

    p_make
}

pkg_install() {
    p_make DESTDIR="$rootfs_cross_root" install
}
