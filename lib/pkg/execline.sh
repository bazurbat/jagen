#!/bin/sh

unset CROSS_COMPILE

jagen_pkg_configure() {
    local sysdeps="$jagen_dir/overlay/toolchain/arm-hisiv200-linux/sysdeps.cfg"
    pkg_run ./configure \
        ${pkg_system:+--host="$pkg_system"} \
        --with-sysdeps="$sysdeps" \
        --with-include="$pkg_sysroot/$pkg_prefix/include"
}
