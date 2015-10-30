#!/bin/sh

pkg_build() {
    # configure fails to run expat test program without this
    CFLAGS="$CFLAGS -I$p_dest_dir$p_prefix/include"
    LDFLAGS="$LDFLAGS -L$p_dest_dir$p_prefix/lib"

    pkg_run ./configure \
        --host="$p_system" \
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

    pkg_run make
}
