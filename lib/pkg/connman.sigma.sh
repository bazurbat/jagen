#!/bin/sh

p_source="$pkg_dist_dir/connman-1.26.tar.xz"

use_env host
use_toolchain target

p_prefix="$target_prefix"
p_dest_dir="$target_dir"

pkg_build() {
    export ac_cv_lib_resolv_ns_initparse=yes

    p_run ./configure \
        --host="$target_system" \
        --prefix="$p_prefix" \
        --sysconfdir="/settings" \
        --localstatedir="/var" \
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
    p_run make DESTDIR="$p_dest_dir" install
}
