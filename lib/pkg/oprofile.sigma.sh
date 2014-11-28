#!/bin/sh

p_source="$pkg_dist_dir/oprofile-0.9.9.tar.gz"

use_toolchain target

pkg_patch() {
    p_patch "${p_source}-AT_BASE_PLATFORM"
}

pkg_build() {
    CFLAGS="$CFLAGS -I$target_dir/$target_prefix/include" \
    p_run ./configure \
        --host="mipsel-linux" \
        --prefix="$target_prefix" \
        --disable-dependency-tracking \
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
