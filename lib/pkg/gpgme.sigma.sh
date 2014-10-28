#!/bin/sh

psource="gpgme-1.5.1"

use_env target

pkg_build() {
    export ac_cv_lib_pthread_pthread_create=no

    p_cmd ./configure \
        --host="mipsel-linux" \
        --prefix="" \
        --includedir="/include" \
        --disable-glibtest \
        --enable-fixed-path="/bin" \
        --disable-gpgconf-test \
        --disable-gpg-test \
        --disable-gpgsm-test \
        --disable-g13-test

    p_cmd make
}

pkg_install() {
    p_cmd make DESTDIR="$rootfs_cross_root" install
}
