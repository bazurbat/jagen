#!/bin/sh

psource="gnupg-1.4.18"

use_env target

pkg_build() {
    p_run ./configure \
        --host="mipsel-linux" \
        --prefix="" \
        --disable-dependency-tracking \
        --enable-minimal \
        --disable-card-support \
        --disable-agent-support \
        --enable-rsa \
        --disable-idea \
        --enable-cast5 \
        --disable-blowfish \
        --disable-aes \
        --disable-twofish \
        --disable-camellia \
        --enable-sha256 \
        --disable-sha512 \
        --disable-bzip2 \
        --disable-exec \
        --disable-photo-viewers \
        --disable-keyserver-helpers \
        --disable-ldap \
        --disable-hkp \
        --disable-finger \
        --disable-generic \
        --disable-mailto \
        --disable-keyserver-path \
        --disable-dns-srv \
        --disable-dns-pka \
        --disable-dns-cert \
        --disable-regex

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$rootfs_cross_root" install
}
