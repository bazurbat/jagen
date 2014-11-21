#!/bin/sh

p_source="$pkg_dist_dir/cryptsetup-1.6.2.tar.bz2"

use_env target

pkg_patch() {
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
        --with-sysroot="$sdk_rootfs_prefix" \
        --with-crypto_backend=gcrypt \
        --with-libgcrypt-prefix="$sdk_rootfs_prefix"

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$sdk_rootfs_prefix" install
}
