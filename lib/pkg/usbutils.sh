#!/bin/sh

use_toolchain target

pkg_prefix=""
pkg_dest_dir="$jagen_sdk_rootfs_prefix"

jagen_pkg_patch() {
    pkg_run sed -ri \
        's:^(pkgconfigdir = ).*$:\1$(libdir)/pkgconfig:g' \
        Makefile.in
}

jagen_pkg_build() {
    export LIBUSB_CFLAGS="-I$jagen_sdk_rootfs_prefix/include/libusb-1.0"
    export LIBUSB_LIBS="-L$jagen_sdk_rootfs_prefix/lib -lusb-1.0"

    pkg_run ./configure \
        --host="$jagen_target_system" \
        --prefix="$pkg_prefix" \
        --disable-zlib \
        --disable-usbids

    pkg_run make
}

jagen_pkg_install() {
    pkg_run make DESTDIR="$pkg_dest_dir" install
}
