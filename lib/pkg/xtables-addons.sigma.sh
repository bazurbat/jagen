#!/bin/sh

p_source="$pkg_dist_dir/xtables-addons-1.47.1.tar.xz"

use_toolchain target

pkg_build() {
    export libxtables_CFLAGS="-I${target_dir}${target_prefix}/include"
    export libxtables_LIBS="-L${target_dir}${target_prefix}/lib"

    p_run ./configure \
        --host="$target_system" \
        --prefix="$target_prefix" \
        --with-kbuild="$LINUX_KERNEL"

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$target_dir" install
}
