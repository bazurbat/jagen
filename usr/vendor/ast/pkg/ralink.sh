#!/bin/sh

jagen_pkg_compile() {
    pkg_run sed -i 's|^\(HAS_WPA_SUPPLICANT=\).*$|\1y|' \
        os/linux/config.mk
    pkg_run sed -i 's|^\(HAS_NATIVE_WPA_SUPPLICANT_SUPPORT=\).*$|\1y|' \
        os/linux/config.mk

    pkg_compile
}

jagen_pkg_install() {
    local cfg_dest="${rootfs_root:?}/etc/Wireless/RT2870STA"

    pkg_run install -vd "$cfg_dest"
    pkg_run install -vm644 RT2870STA.dat "$cfg_dest"

    pkg_install
}
