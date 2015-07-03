#!/bin/sh

pkg_build() {
    export ac_cv_lib_resolv_ns_initparse=yes

    p_run ./configure \
        --host="$p_system" \
        --prefix="$p_prefix" \
        --sysconfdir="/etc" \
        --localstatedir="/settings" \
        --enable-pie \
        --disable-gadget \
        --disable-bluetooth \
        --disable-ofono \
        --disable-dundee \
        --disable-pacrunner \
        --disable-neard \
        --disable-wispr \
        --disable-client

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
