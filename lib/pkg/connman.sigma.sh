#!/bin/sh

p_source="$pkg_dist_dir/connman-1.26.tar.xz"

use_env target

pkg_build() {
    export ac_cv_lib_resolv_ns_initparse=yes
    export GLIB_CFLAGS="-I${target_dir}${target_prefix}/include/glib-2.0 \
-I${target_dir}${target_prefix}/lib/glib-2.0/include"
    export GLIB_LIBS="-L${target_dir}${target_prefix}/lib -lglib-2.0"
    export XTABLES_CFLAGS="-I${target_dir}${target_prefix}/include"
    export XTABLES_LIBS="-L${target_dir}${target_prefix}/lib -lxtables"

    p_run ./configure \
        --host="$target_system" \
        --prefix="$target_prefix" \
        --disable-debug \
        --disable-hh2serial-gps \
        --disable-openconnect \
        --disable-openvpn \
        --disable-vpnc \
        --disable-l2tp \
        --disable-pptp \
        --disable-iospm \
        --disable-tist \
        --disable-session-policy-local \
        --disable-nmcompat \
        --disable-polkit \
        --disable-selinux \
        --disable-loopback \
        --disable-ethernet \
        --disable-gadget \
        --disable-wifi \
        --disable-bluetooth \
        --disable-ofono \
        --disable-dundee \
        --disable-pacrunner \
        --disable-neard \
        --disable-wispr \
        --disable-tools \
        --disable-client \
        --disable-datafiles \
        --with-sysroot="${target_dir}${target_prefix}"

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$target_dir" install
}
