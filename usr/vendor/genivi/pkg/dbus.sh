#!/bin/sh

jagen_pkg_configure_host() {
    pkg_configure \
        "--with-system-pid-file=$pkg_install_dir/run/dbus.pid" \
        "--with-system-socket=$pkg_install_dir/run/dbus/system_bus_socket"
}

jagen_pkg_install_host() {
    pkg_install
    pkg_run mkdir -p "$pkg_install_dir/run/dbus"
}
