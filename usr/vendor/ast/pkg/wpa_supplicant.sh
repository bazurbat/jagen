#!/bin/sh

jagen_pkg_install() {
    local conf_dir="$pkg_install_dir/etc"

    pkg_install

    pkg_run mkdir -p "$conf_dir"
    cat >"$conf_dir/wpa_supplicant.conf" <<"EOF" || return
ctrl_interface=/var/run/wpa_supplicant
ap_scan=1
country=RU
EOF
}
