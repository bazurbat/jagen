#!/bin/sh

p_source="$pkg_dist_dir/sqlite-autoconf-3080403.tar.gz"

use_toolchain target

p_prefix="$target_prefix"
p_dest_dir="$target_dir"

pkg_patch() {
    p_run patch -i "$pkg_dist_dir/patches/sqlite-3.8.1-autoconf-dlopen_check.patch"
    p_run autoreconf -vif
    p_run sed -i 's/sqlite 3\.8\.4\.3/sqlite3.8.4.3/g' configure
}

pkg_build() {
    p_run ./configure \
        --host="$target_system" \
        --prefix="$p_prefix" \
        --disable-static

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$p_dest_dir" install
    p_fix_la "$p_dest_dir$p_prefix/lib/libsqlite3.la" "$p_dest_dir"
}
