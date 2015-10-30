#!/bin/sh

use_toolchain target

pkg_build() {
    CFLAGS="$CFLAGS -I$jagen_target_dir/$jagen_target_prefix/include" \
    p_run ./configure \
        --host="$target_system" \
        --prefix="$jagen_target_prefix" \
        --enable-shared \
        --disable-static \
        --disable-gui \
        --disable-gcov \
        --disable-account-check \
        --without-x \
        --with-extra-includes="$jagen_target_dir/$jagen_target_prefix/include" \
        --with-extra-libs="$jagen_target_dir/$jagen_target_prefix/lib"

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$jagen_target_dir" install
}
