#!/bin/sh

use_toolchain target

p_prefix="$target_prefix"
p_dest_dir="$target_dir"

pkg_patch() {
    if [ -x ./autogen.sh ]; then
        p_run ./autogen.sh
    fi
}

pkg_build() {
    if [ -x ./configure ]; then
        p_run ./configure \
            --host="$target_system" \
            --prefix="$p_prefix"
    fi

    p_run make
}

pkg_install() {
    local dst="$p_dest_dir$p_prefix"

    if [ -x ./configure ]; then
        p_run make DESTDIR="$p_dest_dir" install
    else
        p_run install -vd "$dst/lib"
        p_run cp -vaf include "$dst"
        p_run cp -vaf libuv.so "$dst/lib/libuv.so.0.10.so"
        p_run cd "$dst/lib"
        p_run ln -sf libuv.so.0.10.so libuv.so.0.10
        p_run ln -sf libuv.so.0.10.so libuv.so
    fi
}
