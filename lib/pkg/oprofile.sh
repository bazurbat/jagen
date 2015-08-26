#!/bin/sh

use_toolchain target

pkg_build() {
    CFLAGS="$CFLAGS -I$target_dir/$target_prefix/include" \
    p_run ./configure \
        --host="$target_system" \
        --prefix="$target_prefix" \
        --enable-shared \
        --disable-static \
        --disable-gui \
        --disable-gcov \
        --disable-account-check \
        --without-x \
        --with-extra-includes="$target_dir/$target_prefix/include" \
        --with-extra-libs="$target_dir/$target_prefix/lib"

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$target_dir" install
}
