#!/bin/sh

psource="DPO_RT5572_LinuxSTA_2.6.1.3_20121022"

use_env target

pkg_prepare() {
    p_patch "DPO_RT5572_LinuxSTA_2.6.1.3_20121022-no-tftpboot"
    p_patch "DPO_RT5572_LinuxSTA_2.6.1.3_20121022-encrypt"
}

pkg_build() {
    p_cmd sed -i 's|^\(HAS_WPA_SUPPLICANT=\).*$|\1y|' \
        os/linux/config.mk
    p_cmd sed -i 's|^\(HAS_NATIVE_WPA_SUPPLICANT_SUPPORT=\).*$|\1y|' \
        os/linux/config.mk

    p_make CHIPSET=5370 LINUX_SRC="$LINUX_KERNEL"
}

pkg_install() {
    local dest="$kernelmodulesdir/kernel/drivers/net/wireless"
    local cfg_dest="$rootfs_root/etc/Wireless/RT2870STA"

    p_cmd mkdir -p "$dest"

    p_cmd install -vm644 os/linux/rt5370sta.ko "$dest"

    p_depmod || return $?

    p_cmd install -vd "$cfg_dest"
    p_cmd install -vm644 RT2870STA.dat "$cfg_dest"
}
