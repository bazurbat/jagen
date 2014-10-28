#!/bin/sh

psource="wpa_supplicant-2.2"

use_env target

pkg_prepare() {
    p_patch "wpa_supplicant-2.2-do-not-call-dbus-functions-with-NULL-path"
}

pkg_build() {
    local s="$pbuilddir/$pname"

    cd "$pname" || return $?

    rm -f .config

    echo "BINDIR=/bin"                       >> .config

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

    p_make
}

pkg_install() {
    local s="$pbuilddir/$pname"
    local d="$rootfs_root"

    p_cmd install -vd "$d/bin" "$d/sbin" \
        "$d/etc/dbus-1/system.d" \
        "$d/share/dbus-1/system-services"

    p_cmd install -vm755 "$s/wpa_cli" "$s/wpa_passphrase" "$d/bin"

    p_cmd install -vm755 "$s/wpa_supplicant" "$d/sbin"

    p_cmd install -vm644 \
        "$s/dbus/dbus-wpa_supplicant.conf" \
        "$d/etc/dbus-1/system.d/wpa_supplicant.conf"

    p_cmd install -vm644 \
        "$s/dbus/fi.w1.wpa_supplicant1.service" \
        "$d/share/dbus-1/system-services"

    p_cmd sed -i 's|^\(Exec=\)/bin|\1/sbin|' \
        $d/share/dbus-1/system-services/fi.w1.wpa_supplicant1.service
}
