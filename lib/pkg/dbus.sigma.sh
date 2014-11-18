#!/bin/sh

p_source="$pkg_dist_dir/dbus-1.6.18.tar.gz"

use_env target

pkg_build() {
    p_run ./configure \
        --host="mipsel-linux" \
        --prefix="" \
        --with-system-pid-file=/run/dbus.pid \
        --with-system-socket=/run/dbus/system_bus_socket \
        --disable-compiler-coverage \
        --disable-developer \
        --disable-ansi \
        --disable-verbose-mode \
        --disable-asserts \
        --disable-checks \
        --disable-xml-docs \
        --disable-doxygen-docs \
        --disable-abstract-sockets \
        --disable-selinux \
        --disable-libaudit \
        --disable-dnotify \
        --enable-inotify \
        --disable-kqueue \
        --disable-console-owner-file \
        --disable-userdb-cache \
        --disable-launchd \
        --disable-systemd \
        --disable-embedded-tests \
        --disable-modular-tests \
        --disable-tests \
        --disable-installed-tests \
        --disable-epoll \
        --disable-x11-autolaunch \
        --disable-Werror \
        --disable-stats \
        --with-xml=expat \
        --without-valgrind \
        --without-x

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$sdk_rootfs_prefix" install
}
