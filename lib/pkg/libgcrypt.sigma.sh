#!/bin/sh

use_toolchain target

pkg_patch() {
    p_run autoreconf -vif
}

pkg_build() {
    p_run ./configure \
        --host="$target_system" \
        --prefix="" \
        --disable-dependency-tracking \
        --disable-static \
        --enable-shared \
        --enable-ciphers=cast5,aes \
        --enable-pubkey-ciphers=rsa \
        --enable-digests=sha256 \
        --disable-padlock-support \
        --disable-aesni-support \
        --disable-O-flag-munging \
        --with-sysroot="$sdk_rootfs_prefix"

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$sdk_rootfs_prefix" install
}
