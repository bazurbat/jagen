#!/bin/sh

jagen_pkg_install_host() {
    pkg_install
    pkg_install_file "etc/commonapi-dbus.ini" \
        "${pkg_install_dir:?}/etc/commonapi-dbus.ini"
}
