#!/bin/sh

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

    [ "$pkg_install_prefix" = / ] && pkg_install_prefix=
    pkg_run sed -ri \
        -e "s|^Exec=.*(/wpa_supplicant.*)|Exec=$pkg_install_prefix/sbin\1|" \
        $pkg_install_dir/share/dbus-1/system-services/fi.w1.wpa_supplicant1.service

    pkg_run mkdir -p "$conf_dir"
    cat >"$conf_dir/wpa_supplicant.conf" <<"EOF" || return
ctrl_interface=/var/run/wpa_supplicant
ap_scan=1
country=RU
EOF
}
