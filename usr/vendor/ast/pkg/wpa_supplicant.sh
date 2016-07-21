#!/bin/sh

jagen_pkg_configure_target() {
    rm -f .config

    echo "BINDIR=$pkg_prefix/bin"            >> .config

    echo "CONFIG_CTRL_IFACE=y"               >> .config
    echo "CONFIG_BACKEND=file"               >> .config

    echo "CONFIG_PEERKEY=y"                  >> .config

    echo "CONFIG_BGSCAN_SIMPLE=y"            >> .config
    echo "CONFIG_BGSCAN_LEARN=y"             >> .config

    echo "CONFIG_TLS=internal"               >> .config
    echo "CONFIG_INTERNAL_LIBTOMMATH=y"      >> .config
    echo "CONFIG_INTERNAL_LIBTOMMATH_FAST=y" >> .config

    echo "CONFIG_CTRL_IFACE_DBUS_NEW=y"      >> .config

    case $jagen_sdk in
        hi-linux)
            echo "CFLAGS += $(pkg-config --cflags libnl-3.0)" >> .config
            echo "LIBS   += $(pkg-config --libs libnl-3.0)"   >> .config
            echo "CONFIG_DRIVER_NL80211=y"   >> .config
            echo "CONFIG_LIBNL32=y"          >> .config
            ;;
        *)
            echo "CONFIG_DRIVER_WEXT=y"      >> .config
            ;;
    esac

    echo "CONFIG_DELAYED_MIC_ERROR_REPORT=y" >> .config

    echo "CONFIG_IEEE8021X_EAPOL=y"          >> .config
}

jagen_pkg_compile_target() {
    use_env target_toolchain

    pkg_run make
}

jagen_pkg_install() {
    local conf_dir="$pkg_install_dir/etc"

    pkg_run install -vd \
        "$pkg_install_dir/bin"                 \
        "$pkg_install_dir/sbin"                \
        "$pkg_install_dir/etc/dbus-1/system.d" \
        "$pkg_install_dir/share/dbus-1/system-services"

    pkg_run install -vm 755 \
        "$pkg_build_dir/wpa_cli" \
        "$pkg_build_dir/wpa_passphrase" \
        "$pkg_install_dir/bin"

    pkg_run install -vm 755 \
        "$pkg_build_dir/wpa_supplicant" \
        "$pkg_install_dir/sbin"

    pkg_run install -vm 644 \
        "$pkg_build_dir/dbus/dbus-wpa_supplicant.conf" \
        "$pkg_install_dir/etc/dbus-1/system.d/wpa_supplicant.conf"

    pkg_run install -vm 644 \
        "$pkg_build_dir/dbus/fi.w1.wpa_supplicant1.service" \
        "$pkg_install_dir/share/dbus-1/system-services"

    [ "$pkg_prefix" = / ] && pkg_prefix=
    pkg_run sed -ri \
        -e "s|^Exec=.*(/wpa_supplicant.*)|Exec=$pkg_prefix/sbin\1|" \
        $pkg_install_dir/share/dbus-1/system-services/fi.w1.wpa_supplicant1.service

    pkg_run mkdir -p "$conf_dir"
    cat >"$conf_dir/wpa_supplicant.conf" <<"EOF" || return
ctrl_interface=/var/run/wpa_supplicant
ap_scan=1
country=RU
EOF
}
