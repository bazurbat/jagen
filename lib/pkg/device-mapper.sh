#!/bin/sh

use_toolchain target

jagen_pkg_build() {
    pkg_run ./configure \
        --host="$jagen_target_system" \
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
        --disable-selinux

    pkg_run make device-mapper
}

jagen_pkg_install() {
    pkg_run make DESTDIR="$sdk_rootfs_prefix" install_device-mapper
    pkg_run chmod 755 "$sdk_rootfs_prefix"/lib/libdevmapper*
    pkg_run chmod 755 "$sdk_rootfs_prefix"/sbin/dmsetup*
}
