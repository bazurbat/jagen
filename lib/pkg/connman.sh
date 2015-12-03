#!/bin/sh

jagen_pkg_build() {
    export ac_cv_lib_resolv_ns_initparse=yes

    pkg_run ./configure \
        --host="$pkg_system" \
        --prefix="$pkg_prefix" \
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

    pkg_run make
}

install_dbus_conf() {
    local conf_path="/etc/dbus-1/system.d"

    pkg_run install -vd "$jagen_sdk_rootfs_root$conf_path"
    pkg_run install -vm 644 \
        "$jagen_target_dir$conf_path/connman.conf" \
        "$jagen_sdk_rootfs_root$conf_path"
}

jagen_pkg_install() {
    pkg_run make DESTDIR="$pkg_dest_dir" install
}
