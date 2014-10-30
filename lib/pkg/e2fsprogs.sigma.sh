#!/bin/sh

psource="$pkg_distdir/e2fsprogs-1.42.9.tar.gz"

use_env target

pkg_prepare() {
    p_patch "e2fsprogs-1.41.8-makefile"
    p_run rm -rf doc
}

pkg_build() {
# ac_cv_lib_uuid_uuid_generate=yes \
# ac_cv_lib_blkid_blkid_get_cache=yes \
# to use from util-linux

    CFLAGS="$CFLAGS -D_GNU_SOURCE" \
    ac_cv_path_LDCONFIG=: \
    QUOTA_CMT='#' \
    p_run ./configure \
        --prefix="" \
        --host="mipsel-linux" \
        --enable-symlink-install \
        --enable-relative-symlinks \
        --disable-compression \
        --enable-htree \
        --disable-elf-shlibs \
        --disable-bsd-shlibs \
        --disable-profile \
        --disable-checker \
        --disable-jbd-debug \
        --disable-blkid-debug \
        --disable-testio-debug \
        --enable-libuuid \
        --enable-libblkid \
        --disable-quota \
        --disable-backtrace \
        --disable-debugfs \
        --disable-imager \
        --disable-resizer \
        --disable-defrag \
        --enable-fsck \
        --disable-e2initrd-helper \
        --disable-tls \
        --disable-uuidd \
        --disable-nls \
        --disable-rpath \
        --with-root-prefix=""

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$sdk_rootfs_prefix" install
}
