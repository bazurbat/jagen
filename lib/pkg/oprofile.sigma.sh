#!/bin/sh

psource="$pkg_distdir/oprofile-0.9.9.tar.gz"

use_env target

pkg_prepare() {
    p_patch "${psource}-AT_BASE_PLATFORM"
}

pkg_build() {
    CFLAGS="$CFLAGS -I$targetdir/$targetprefix/include" \
    p_run ./configure \
        --host="mipsel-linux" \
        --prefix="$targetprefix" \
        --disable-dependency-tracking \
        --enable-shared \
        --disable-static \
        --disable-gui \
        --disable-gcov \
        --disable-account-check \
        --without-x \
        --with-extra-includes="$targetdir/$targetprefix/include" \
        --with-extra-libs="$targetdir/$targetprefix/lib"

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$targetdir" install
}
