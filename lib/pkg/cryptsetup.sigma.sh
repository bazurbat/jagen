#!/bin/sh

psource="cryptsetup-1.6.2"

use_env target

pkg_prepare() {
    p_run autoreconf -vif
}

pkg_build() {
    p_run ./configure \
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

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$rootfs_cross_root" install
}
