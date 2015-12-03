#!/bin/sh

use_toolchain target

jagen_pkg_build() {
    # NOTE: alsa utils fail with link errors without ucm
    pkg_run ./configure \
        --host="$jagen_target_system" \
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

    pkg_run make
}

jagen_pkg_install() {
    pkg_run make DESTDIR="$jagen_sdk_rootfs_prefix" install
}
