#!/bin/sh

pworkdir="$rootfsdir"

pkg_unpack() {
    use_env tools

    p_run rm -rf "build_mipsel" "cross_rootfs"
    [ -d dl ] || p_run cp -r "$distdir/dl" "$rootfsdir"
    # p_run cd "package/target-shell/busybox"
    # p_run make clean
}

pkg_build() {
    use_env tools

    if [ "$ja_buildtype" = Release ]; then
        cp config.release .config
    else
        cp config.dev .config
    fi

    p_run make

    # contains cyclic symlinks
    rm -rf "package/udev/udev-114/test/sys"
}

create_dirs() {
    cd "$rootfs_root" &&
    rm -rf dev opt proc sys root tmp var/run &&
    install -m 700 -d root &&
    if [ ! -d libexec ]
    then mkdir libexec
    fi
}

fix_init_link() {
    cd "$rootfs_root" &&
    rm -f init linuxrc &&
    ln -s /bin/busybox init
}

fix_xenv_bins() {
    cd "$rootfs_root/bin" &&
    rm -f setxenv setxenv.bash unsetxenv &&
    ln -sf setxenv2_mipsel setxenv2 &&
    ln -sf setxenv2_mipsel unsetxenv2
}

clean_etc() {
    cd "$rootfs_root/etc" &&
    rm -rf init.d rc.d network cs_rootfs_* &&
    rm -f ld.so.cache mtab
    for d in up down pre-up post-down; do
        mkdir -p network/if-${d}.d
    done
}

remove_nss_libs() {
    cd "$rootfs_root/lib" &&
    rm -f libnss_compat* libnss_hesiod* libnss_nis*
}

remove_ncurses() {
    cd "$rootfs_root" &&
    rm -f etc/inputrc &&
    rm -rf usr/share/terminfo &&
    cd "$rootfs_root/lib" &&
    rm -f libmenu* libpanel* libform* libncurses*
}

remove_image_libs() {
    cd "$rootfs_root/lib" || return $?
    rm -f libjpeg*
    rm -f libpng*
    rm -f libtiff*
    rm -f libungif*
}

remove_ssl() {
    cd "$rootfs_root" || return $?
    rm -f lib/libssl* lib/libcrypto*
    rm -f usr/bin/openssl
}

remove_libcurl() {
    cd "$rootfs_root/lib" || return $?
    rm -f libcurl*
}

install_keys() {
    mkdir -p "$rootfs_root/lib/firmware" || return $?
    cp -a "$ja_files_dir/keys/keyfile.gpg" "$rootfs_root/lib/firmware"
}

install_gpg() {
    cd "$rootfs_cross_root" || return $?
    cp -a bin/gpg "$rootfs_root/bin" || return $?
    cp -a lib/libgpg* lib/libassuan* "$rootfs_root/lib"
}

install_util_linux() {
    cd "$rootfs_cross_root" || return $?
    cp -a sbin/losetup "$rootfs_root/sbin"
}

install_e2fsprogs() {
    cd "$rootfs_cross_root/sbin" || return $?
    cp -a badblocks blkid e2fsck "$rootfs_root/sbin" || return $?
    if [ "$rootfs_add_e2fs_tools" = "yes" ]; then
        cp -a dumpe2fs mke2fs tune2fs "$rootfs_root/sbin" || return $?
    fi
}

install_cryptsetup() {
    cd "$rootfs_cross_root" || return $?
    cp -af lib/libcryptsetup* lib/libuuid* lib/libdevmapper* lib/libgcrypt* \
        lib/libpopt* "$rootfs_root/lib"
    cp -af sbin/cryptsetup "$rootfs_root/sbin"
}

install_freetype() {
    cd "$rootfs_cross_root" || return $?
    cp -af lib/libfreetype*.so* "$rootfs_root/lib"
}

install_dbus() {
    cd "$rootfs_cross_root" || return $?
    cp -a bin/dbus-* "$rootfs_root/bin" || return $?
    cp -a "etc/dbus-1" "$rootfs_root/etc" || return $?
    cp -a lib/libexpat* lib/libdbus* "$rootfs_root/lib" || return $?
    cp -a "libexec/dbus-daemon-launch-helper" "$rootfs_root/libexec"
}

install_rsync() {
    cd "$rootfs_cross_root" || return $?
    cp -a bin/rsync "$rootfs_root/bin" || return $?
}

install_libuv() {
    cd "$rootfs_cross_root" || return $?
    cp -a lib/libuv.so* "$rootfs_root/lib" || return $?
}

install_chibi() {
    cd "$rootfs_cross_root" || return $?
    cp -a bin/chibi-scheme "$rootfs_root/bin" || return $?
    cp -a lib/*chibi* "$rootfs_root/lib" || return $?
    cp -a share/chibi "$rootfs_root/share" || return $?
}

install_ldconfig() {
    cd "$TOOLCHAIN_RUNTIME_PATH" || return $?
    cp -a sbin/ldconfig "$rootfs_root/sbin"
}

install_zoneinfo() {
    mkdir -p "$rootfs_root/usr/share/zoneinfo" &&
    cd "${TOOLCHAIN_RUNTIME_PATH}/usr/share/zoneinfo" &&
    cp -r Etc Europe Factory GMT UTC "$rootfs_root/usr/share/zoneinfo" &&
    cd "$rootfs_root/etc" &&
    rm -f TZ &&
    ln -sf /usr/share/zoneinfo/GMT localtime
}

install_gdbserver() {
    # GDB Server
    cp -f "$rootfs_cross_root/bin/gdbserver" \
        "$rootfs_root/bin"
}

clean_misc() {
    cd "$rootfs_root" || return $?

    rm -f bin/mtd_*

    if [ "$ja_buildtype" = "Release" ]; then
        rm -rf usr/local || return $?
    fi

    find lib usr/lib \( -name "*.a" -o -name "*.la" \) -delete

    remove_nss_libs || return $?

    if [ "$ja_buildtype" = "Release" ]; then
        remove_ncurses
    fi
}

install_files() {
    cp -rf "$ja_files_dir"/rootfs/* "$rootfs_root"
}

pkg_install() {
    use_env target

    create_dirs || die "create_dirs failed"
    fix_init_link || die "fix_init_link failed"
    fix_xenv_bins || die "fix_xenv_bins failed"
    clean_etc || die "clean_etc failed"
    install_keys || die "install_keys failed"
    install_gpg || die "install_gpg failed"
    install_util_linux || die "install_util_linux failed"
    install_e2fsprogs || die "install_e2fsprogs failed"
    # install_cryptsetup || return $?
    install_freetype || die "install_freetype failed"
    install_dbus || die "install_dbus failed"
    install_rsync || die "install_rsync failed"
    install_libuv || die "install_libuv failed"
    # install_chibi || die "install_chibi failed"
    install_ldconfig || die "install_ldconfig failed"
    install_zoneinfo || die "install_zoneinfo failed"
    clean_misc || die "clean_misc failed"
    remove_image_libs || die "remove_image_libs failed"

    if [ "$sdkver" != "3.11" ]; then
        remove_ssl || die "remove_ssl failed"
        remove_libcurl || die "remove_libcurl failed"
    fi

    install_files || die "install_files failed"

    p_strip "$rootfs_root" >>"$plog" 2>&1 || die "strip failed"
}
