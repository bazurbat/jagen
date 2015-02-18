#!/bin/sh

use_toolchain target

p_prefix=""
p_dest_dir="$sdk_rootfs_prefix"

pkg_patch() {
    p_run autoreconf -if
}

pkg_build() {
    p_run ./configure \
        --host="$target_system" \
        --prefix="$p_prefix" \
        --disable-shared \
        --disable-static \
        --disable-nls \
        --disable-rpath \
        --disable-most-builds \
        --disable-libuuid \
        --disable-libblkid \
        --disable-libmount \
        --disable-mount \
        --enable-losetup \
        --disable-cytune \
        --disable-fsck \
        --disable-partx \
        --disable-uuidd \
        --disable-mountpoint \
        --disable-fallocate \
        --disable-unshare \
        --disable-nsenter \
        --disable-setpriv \
        --disable-eject \
        --disable-agetty \
        --disable-cramfs \
        --disable-bfs \
        --disable-fdformat \
        --disable-hwclock \
        --disable-wdctl \
        --disable-switch_root \
        --disable-pivot_root \
        --disable-elvtune \
        --disable-tunelp \
        --disable-kill \
        --disable-last \
        --disable-utmpdump \
        --disable-line \
        --disable-mesg \
        --disable-raw \
        --disable-rename \
        --disable-reset \
        --disable-vipw \
        --disable-newgrp \
        --disable-chfn-chsh \
        --disable-login \
        --disable-sulogin \
        --disable-su \
        --disable-runuser \
        --disable-ul \
        --disable-more \
        --disable-pg \
        --disable-setterm \
        --disable-schedutils \
        --disable-wall \
        --disable-write \
        --disable-socket-activation \
        --disable-bash-completion \
        --disable-pg-bell \
        --disable-makeinstall-chown \
        --disable-makeinstall-setuid \
        --without-selinux \
        --without-audit \
        --without-udev \
        --without-ncurses \
        --without-slang \
        --without-utempter \
        --without-python

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$p_dest_dir" install
}
