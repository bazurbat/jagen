#!/bin/sh

p_source="$pkg_dist_dir/gpgme-1.5.1.tar.bz2"

use_env target

pkg_build() {
    export ac_cv_lib_pthread_pthread_create=no

    p_run ./configure \
        --host="mipsel-linux" \
        --prefix="" \
        --includedir="/include" \
        --disable-glibtest \
        --enable-fixed-path="/bin" \
        --disable-gpgconf-test \
        --disable-gpg-test \
        --disable-gpgsm-test \
        --disable-g13-test

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$sdk_rootfs_prefix" install
}
