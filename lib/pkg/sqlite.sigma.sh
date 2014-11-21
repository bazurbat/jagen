#!/bin/sh

p_source="$pkg_dist_dir/sqlite-autoconf-3080403.tar.gz"

use_env target

pkg_patch() {
    p_run patch -i "$pkg_dist_dir/patches/sqlite-3.8.1-autoconf-dlopen_check.patch"
    p_run autoreconf -vif
    p_run sed -i 's/sqlite 3\.8\.4\.3/sqlite3.8.4.3/g' configure
}

pkg_build() {
    p_run ./configure \
        --host="mipsel-linux" \
        --prefix="" \
        --disable-static

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$sdk_rootfs_prefix" install
}
