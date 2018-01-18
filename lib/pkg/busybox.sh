#!/bin/sh

jagen_pkg_patch() {
    local config="$(find_in_path etc/busybox.config)"

    if [ -f "$config" ]; then
        pkg_run cp -vf "$config" .config
    else
        die "Could not find BusyBox config for current configuration"
    fi
}

jagen_pkg_compile() {
    pkg_run make oldconfig
    pkg_run make
}

jagen_pkg_install() {
    pkg_run make CONFIG_PREFIX="$pkg_install_dir" install
}
