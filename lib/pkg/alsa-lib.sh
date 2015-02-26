#!/bin/sh

use_toolchain target

pkg_build() {
    # NOTE: alsa utils fail with link errors without ucm
    p_run ./configure \
        --host="$target_system" \
        --prefix="" \
        --disable-maintainer-mode \
        --enable-shared \
        --disable-debug \
        --disable-resmgr \
        --disable-aload \
        --enable-mixer \
        --enable-pcm \
        --disable-rawmidi \
        --disable-hwdep \
        --disable-seq \
        --enable-ucm \
        --disable-alisp \
        --disable-old-symbols \
        --disable-python \
        --with-pcm-plugins=hw \
        --without-versioned \
        --with-debug

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$sdk_rootfs_prefix" install
}
