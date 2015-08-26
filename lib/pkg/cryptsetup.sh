#!/bin/sh

use_toolchain target

pkg_patch() {
    p_autoreconf
}

pkg_build() {
    p_run ./configure \
        --host="$target_system" \
        --prefix="" \
        --disable-static \
        --enable-shared \
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
