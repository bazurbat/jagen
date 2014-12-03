#!/bin/sh

p_source="$pkg_dist_dir/LVM2.2.02.95.tgz"

use_toolchain target

pkg_build() {
    p_run ./configure \
        --host="$target_system" \
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
    p_run make DESTDIR="$sdk_rootfs_prefix" install_device-mapper
    p_run chmod 755 "$sdk_rootfs_prefix"/lib/libdevmapper*
    p_run chmod 755 "$sdk_rootfs_prefix"/sbin/dmsetup*
}
