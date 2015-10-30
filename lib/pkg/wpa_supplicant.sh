#!/bin/sh

jagen_pkg_build() {
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

    if in_flags new_kernel; then
        echo "CFLAGS += $(pkg-config --cflags libnl-3.0)" >> .config
        echo "LIBS   += $(pkg-config --libs libnl-3.0)"   >> .config
        echo "CONFIG_DRIVER_NL80211=y"       >> .config
        echo "CONFIG_LIBNL32=y"              >> .config
    else
        echo "CONFIG_DRIVER_WEXT=y"          >> .config
    fi

    echo "CONFIG_DELAYED_MIC_ERROR_REPORT=y" >> .config

    echo "CONFIG_IEEE8021X_EAPOL=y"          >> .config

    pkg_run make
}

jagen_pkg_install() {
    local s="$p_build_dir/$p_name"
    local d="$p_dest_dir$p_prefix"

    pkg_run install -vd "$d/bin" "$d/sbin" \
        "$d/etc/dbus-1/system.d" \
        "$d/share/dbus-1/system-services"

    pkg_run install -vm755 "$s/wpa_cli" "$s/wpa_passphrase" "$d/bin"

    pkg_run install -vm755 "$s/wpa_supplicant" "$d/sbin"

    pkg_run install -vm644 \
        "$s/dbus/dbus-wpa_supplicant.conf" \
        "$d/etc/dbus-1/system.d/wpa_supplicant.conf"

    pkg_run install -vm644 \
        "$s/dbus/fi.w1.wpa_supplicant1.service" \
        "$d/share/dbus-1/system-services"

    pkg_run sed -ri \
        -e "s|^Exec=.*(/wpa_supplicant.*)|Exec=$p_prefix/sbin\1|" \
        $d/share/dbus-1/system-services/fi.w1.wpa_supplicant1.service
}
