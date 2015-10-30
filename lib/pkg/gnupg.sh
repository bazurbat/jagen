#!/bin/sh

use_toolchain target

pkg_build() {
    pkg_run ./configure \
        --host="$target_system" \
        --prefix="" \
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

    pkg_run make
}

pkg_install() {
    pkg_run make DESTDIR="$sdk_rootfs_prefix" install
}
