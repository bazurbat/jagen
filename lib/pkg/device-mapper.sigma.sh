#!/bin/sh

psource="LVM2.2.02.95"

use_env target

pkg_build() {
    p_run ./configure \
        --host="mipsel-linux" \
        --prefix="" \
        --disable-lvm1_fallback \
        --disable-readline \
        --disable-ocf \
        --disable-cmirrord \
        --disable-debug \
        --disable-profiling \
        --disable-testing \
        --disable-valgrind-pool \
        --enable-devmapper \
        --disable-lvmetad \
        --disable-udev_sync \
        --disable-udev_rules \
        --disable-compat \
        --disable-units-compat \
        --enable-o_direct \
        --disable-applib \
        --disable-cmdlib \
        --enable-pkgconfig \
        --disable-write_install \
        --disable-fsadm \
        --disable-dmeventd \
        --disable-selinux \
        --disable-nls

    p_run make device-mapper
}

pkg_install() {
    p_run make DESTDIR="$rootfs_cross_root" install_device-mapper
    p_run chmod 755 "$rootfs_cross_root"/lib/libdevmapper*
    p_run chmod 755 "$rootfs_cross_root"/sbin/dmsetup*
}
