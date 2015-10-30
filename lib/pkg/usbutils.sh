#!/bin/sh

use_toolchain target

p_prefix=""
p_dest_dir="$sdk_rootfs_prefix"

pkg_patch() {
    pkg_run sed -ri \
        's:^(pkgconfigdir = ).*$:\1$(libdir)/pkgconfig:g' \
        Makefile.in
}

pkg_build() {
    export LIBUSB_CFLAGS="-I$sdk_rootfs_prefix/include/libusb-1.0"
    export LIBUSB_LIBS="-L$sdk_rootfs_prefix/lib -lusb-1.0"

    pkg_run ./configure \
        --host="$target_system" \
        --prefix="$p_prefix" \
        --disable-zlib \
        --disable-usbids

    pkg_run make
}

pkg_install() {
    pkg_run make DESTDIR="$p_dest_dir" install
}
