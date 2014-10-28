#!/bin/sh

psource="libgcrypt-1.5.3"

use_env target

pkg_prepare() {
    p_patch "libgcrypt-1.5.0-uscore"
    p_patch "libgcrypt-multilib-syspath"
    p_run autoreconf -vif
}

pkg_build() {
    p_run ./configure \
        --host="mipsel-linux" \
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
        --with-sysroot="$rootfs_cross_root"

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$rootfs_cross_root" install
}
