#!/bin/sh

use_toolchain target

p_prefix=""
p_dest_dir="$sdk_rootfs_prefix"

pkg_build() {
    export ac_cv_lib_pthread_pthread_create=no

    p_run ./configure \
        --host="$target_system" \
        --prefix="" \
        --includedir="/include" \
        --disable-glibtest \
        --enable-fixed-path="/bin" \
        --disable-gpgconf-test \
        --disable-gpg-test \
        --disable-gpgsm-test \
        --disable-g13-test \
        --with-sysroot="$p_dest_dir"

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$p_dest_dir" install
    p_fix_la "$p_dest_dir/lib/libgpgme.la"
}
