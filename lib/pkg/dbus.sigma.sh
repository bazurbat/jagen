#!/bin/sh

p_source="$pkg_dist_dir/dbus-1.6.18.tar.gz"

use_toolchain target

p_prefix="$target_prefix"
p_dest_dir="$target_dir"

pkg_build() {
    # configure fails to run expat test program without this
    CFLAGS="$CFLAGS -I$p_dest_dir$p_prefix/include"
    LDFLAGS="$LDFLAGS -L$p_dest_dir$p_prefix/lib"

    p_run ./configure \
        --host="$target_system" \
        --prefix="$p_prefix" \
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
    p_run make DESTDIR="$p_dest_dir" install
}
