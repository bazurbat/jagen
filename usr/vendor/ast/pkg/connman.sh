#!/bin/sh

jagen_pkg_install() {
    local conf_dir="$pkg_install_dir/etc/connman"

    pkg_install

    pkg_run mkdir -p "$conf_dir"
    cat >"$conf_dir/main.conf" <<"EOF" || return
[General]
PreferredTechnologies = ethernet,wifi
SingleConnectedTechnology = true
EOF
}
