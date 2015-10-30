#!/bin/sh

use_toolchain target

pkg_build() {
    pkg_run sed -i 's|^\(HAS_WPA_SUPPLICANT=\).*$|\1y|' \
        os/linux/config.mk
    pkg_run sed -i 's|^\(HAS_NATIVE_WPA_SUPPLICANT_SUPPORT=\).*$|\1y|' \
        os/linux/config.mk

    pkg_run make CROSS_COMPILE="${toolchain_bin_dir}/${target_system}-" \
        CHIPSET=5370 LINUX_SRC="$LINUX_KERNEL"
}

pkg_install() {
    local dest="$kernel_modules_dir/kernel/drivers/net/wireless"
    local cfg_dest="$sdk_rootfs_root/etc/Wireless/RT2870STA"

    pkg_run mkdir -p "$dest"

    pkg_run install -vm644 os/linux/rt5370sta.ko "$dest"

    pkg_run_depmod || return $?

    pkg_run install -vd "$cfg_dest"
    pkg_run install -vm644 RT2870STA.dat "$cfg_dest"
}
