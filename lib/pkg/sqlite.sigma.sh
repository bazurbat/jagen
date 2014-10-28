#!/bin/sh

psource="sqlite-autoconf-3080403"

use_env target

pkg_prepare() {
    p_run patch -i "$distdir/patches/sqlite-3.8.1-autoconf-dlopen_check.patch"
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
    p_run make DESTDIR="$rootfs_cross_root" install
}
