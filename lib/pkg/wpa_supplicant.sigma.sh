#!/bin/sh

p_source="$pkg_dist_dir/wpa_supplicant-2.2.tar.gz"

use_toolchain target

p_prefix="$target_prefix"
p_dest_dir="$target_dir"

pkg_patch() {
    p_patch "wpa_supplicant-2.2-do-not-call-dbus-functions-with-NULL-path"
}

pkg_build() {
    local s="$p_build_dir/$p_name"

    cd "$p_name" || return $?

    rm -f .config

    echo "BINDIR=$p_prefix/bin"              >> .config

    echo "CONFIG_CTRL_IFACE=y"               >> .config
    echo "CONFIG_BACKEND=file"               >> .config

    echo "CONFIG_PEERKEY=y"                  >> .config

    echo "CONFIG_BGSCAN_SIMPLE=y"            >> .config
    echo "CONFIG_BGSCAN_LEARN=y"             >> .config

    echo "CONFIG_TLS=internal"               >> .config
    echo "CONFIG_INTERNAL_LIBTOMMATH=y"      >> .config
    echo "CONFIG_INTERNAL_LIBTOMMATH_FAST=y" >> .config

    echo "CONFIG_CTRL_IFACE_DBUS_NEW=y"      >> .config

    echo "CONFIG_DRIVER_WEXT=y"              >> .config

    echo "CONFIG_DELAYED_MIC_ERROR_REPORT=y" >> .config

    echo "CONFIG_IEEE8021X_EAPOL=y"          >> .config

    p_run make
}

pkg_install() {
    local s="$p_build_dir/$p_name"
    local d="$p_dest_dir"

    p_run install -vd "$d/bin" "$d/sbin" \
        "$d/etc/dbus-1/system.d" \
        "$d/share/dbus-1/system-services"

    p_run install -vm755 "$s/wpa_cli" "$s/wpa_passphrase" "$d/bin"

    p_run install -vm755 "$s/wpa_supplicant" "$d/sbin"

    p_run install -vm644 \
        "$s/dbus/dbus-wpa_supplicant.conf" \
        "$d/etc/dbus-1/system.d/wpa_supplicant.conf"

    p_run install -vm644 \
        "$s/dbus/fi.w1.wpa_supplicant1.service" \
        "$d/share/dbus-1/system-services"

    p_run sed -i 's|^\(Exec=\)/bin|\1/sbin|' \
        $d/share/dbus-1/system-services/fi.w1.wpa_supplicant1.service
}
