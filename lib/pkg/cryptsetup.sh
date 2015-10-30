#!/bin/sh

use_toolchain target

pkg_patch() {
    p_autoreconf
}

pkg_build() {
    pkg_run ./configure \
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

    pkg_run make
}

pkg_install() {
    pkg_run make DESTDIR="$sdk_rootfs_prefix" install
}
