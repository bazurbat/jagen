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
        --enable-ciphers=cast5,aes \
        --enable-pubkey-ciphers=rsa \
        --enable-digests=sha256 \
        --disable-padlock-support \
        --disable-aesni-support \
        --disable-O-flag-munging \
        --with-sysroot="$sdk_rootfs_prefix"

    pkg_run make
}

pkg_install() {
    pkg_run make DESTDIR="$sdk_rootfs_prefix" install
}
