#!/bin/sh

jagen_pkg_configure() {
    local config=".config"

    cat >"$config" <<EOF || return
CONFIG_DRIVER_HOSTAP=y
CONFIG_DRIVER_NL80211=y
CONFIG_LIBNL32=y
CFLAGS += $(pkg-config --cflags libnl-3.0)
LIBS += $(pkg-config --libs libnl-3.0)
CONFIG_NO_ACCOUNTING=y
CONFIG_NO_RADIUS=y
CONFIG_NO_VLAN=y
CONFIG_NO_DUMP_STATE=y
CONFIG_ELOOP_EPOLL=y
CONFIG_TLS=internal
CONFIG_INTERNAL_LIBTOMMATH=y
CONFIG_INTERNAL_LIBTOMMATH_FAST=y
EOF
}

jagen_pkg_install() {
    pkg_run install -vm755 hostapd "$pkg_install_dir/sbin"
    pkg_run install -vm755 hostapd_cli "$pkg_install_dir/bin"
}
