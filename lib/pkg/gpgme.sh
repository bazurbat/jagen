#!/bin/sh

use_toolchain target

pkg_prefix=""
pkg_dest_dir="$jagen_sdk_rootfs_prefix"

jagen_pkg_build() {
    export ac_cv_lib_pthread_pthread_create=no

    pkg_run "$pkg_source_dir/configure" \
        --host="$jagen_target_system" \
        --prefix="" \
        --includedir="/include" \
        --disable-glibtest \
        --enable-fixed-path="/bin" \
        --disable-gpgconf-test \
        --disable-gpg-test \
        --disable-gpgsm-test \
        --disable-g13-test \
        --with-sysroot="$pkg_dest_dir"

    pkg_run make
}

jagen_pkg_install() {
    pkg_run make DESTDIR="$pkg_dest_dir" install
    pkg_fix_la "$pkg_dest_dir/lib/libgpgme.la"
}
