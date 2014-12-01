#!/bin/sh

p_source="$pkg_dist_dir/connman-1.26.tar.xz"

use_toolchain target

PKG_CONFIG_SYSROOT_DIR="$target_dir"

pkg_build() {
    export ac_cv_lib_resolv_ns_initparse=yes

    export DBUS_CFLAGS="-I$sdk_rootfs_prefix/lib/dbus-1.0/include -I$sdk_rootfs_prefix/include/dbus-1.0"
    export DBUS_LIBS="-L$sdk_rootfs_prefix/lib -ldbus-1"

#     export GLIB_CFLAGS="-I${target_dir}${target_prefix}/include/glib-2.0 \
# -I${target_dir}${target_prefix}/lib/glib-2.0/include"
#     export GLIB_LIBS="-L${target_dir}${target_prefix}/lib -lglib-2.0"
#     export XTABLES_CFLAGS="-I${target_dir}${target_prefix}/include"
#     export XTABLES_LIBS="-L${target_dir}${target_prefix}/lib -lxtables"

    p_run ./configure \
        --host="$target_system" \
        --prefix="$target_prefix" \
        --enable-pie \
        --disable-gadget \
        --disable-bluetooth \
        --disable-ofono \
        --disable-dundee \
        --disable-pacrunner \
        --disable-neard \
        --disable-wispr \
        --disable-client \
        --with-sysroot="${target_dir}${target_prefix}"

    p_run make
}

install_dbus_conf() {
    local conf_path="/etc/dbus-1/system.d"

    p_run install -vd "$sdk_rootfs_root$conf_path"
    p_run install -vm 644 \
        "$target_dir$conf_path/connman.conf" \
        "$sdk_rootfs_root$conf_path"
}

pkg_install() {
    p_run make DESTDIR="$target_dir" install
    install_dbus_conf
}
