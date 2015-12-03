#!/bin/sh

use_toolchain target

jagen_pkg_patch() {
    pkg_run rm -rf doc
}

jagen_pkg_build() {
# ac_cv_lib_uuid_uuid_generate=yes \
# ac_cv_lib_blkid_blkid_get_cache=yes \
# to use from util-linux

    CFLAGS="$CFLAGS -D_GNU_SOURCE" \
    ac_cv_path_LDCONFIG=: \
    QUOTA_CMT='#' \
    pkg_run ./configure \
        --prefix="" \
        --host="$jagen_target_system" \
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
        --disable-rpath \
        --with-root-prefix=""

    pkg_run make
}

jagen_pkg_install() {
    pkg_run make DESTDIR="$sdk_rootfs_prefix" install
}
