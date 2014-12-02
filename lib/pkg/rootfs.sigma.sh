#!/bin/sh

p_source="git git@bitbucket.org:art-system/sigma-rootfs.git"
p_source_dir="$pkg_src_dir/sigma-rootfs"
p_jobs=1

[ -d "$SMP86XX_TOOLCHAIN_PATH" ] ||
    die "SMP86XX_TOOLCHAIN_PATH ($SMP86XX_TOOLCHAIN_PATH) is not found"

pkg_patch() {
    [ -d dl ] || p_src_copy "$pkg_dist_dir/dl" "$p_build_dir/dl"
}

pkg_build() {
    use_env tools

    if [ "$pkg_build_type" = Release ]; then
        cp config.release .config
    else
        cp config.dev .config
    fi

    p_run make

    # contains cyclic symlinks
    rm -rf "package/udev/udev-114/test/sys"
}

create_dirs() {
    p_run cd "$sdk_rootfs_root"
    rm -rf dev opt proc sys root tmp usr var/run &&
    install -m 700 -d root
}

fix_init_link() {
    cd "$sdk_rootfs_root" &&
    rm -f init linuxrc &&
    ln -s /bin/busybox init
}

fix_xenv_bins() {
    cd "$sdk_rootfs_root/bin" &&
    rm -f setxenv setxenv.bash unsetxenv &&
    ln -sf setxenv2_mipsel setxenv2 &&
    ln -sf setxenv2_mipsel unsetxenv2
}

clean_etc() {
    cd "$sdk_rootfs_root/etc" &&
    rm -rf init.d rc.d network cs_rootfs_* &&
    rm -f ld.so.cache mtab
    for d in up down pre-up post-down; do
        mkdir -p network/if-${d}.d
    done

    p_run rm -f "$sdk_rootfs_root/etc/TZ"
    p_run install -m644 \
        "$TOOLCHAIN_RUNTIME_PATH/usr/share/zoneinfo/GMT" \
        "$sdk_rootfs_root/etc/localtime"
}

remove_nss_libs() {
    cd "$sdk_rootfs_root/lib" &&
    rm -f libnss_compat* libnss_hesiod* libnss_nis*
}

remove_ncurses() {
    cd "$sdk_rootfs_root" &&
    rm -f etc/inputrc &&
    rm -rf usr/share/terminfo &&
    cd "$sdk_rootfs_root/lib" &&
    rm -f libmenu* libpanel* libform* libncurses*
}

remove_image_libs() {
    cd "$sdk_rootfs_root/lib" || return $?
    rm -f libjpeg*
    rm -f libpng*
    rm -f libtiff*
    rm -f libungif*
}

install_keys() {
    mkdir -p "$sdk_rootfs_root/lib/firmware" || return $?
    cp -a "$pkg_private_dir/keys/keyfile.gpg" "$sdk_rootfs_root/lib/firmware"
}

install_gpg() {
    cd "$sdk_rootfs_prefix" || return $?
    cp -a bin/gpg "$sdk_rootfs_root/bin" || return $?
    cp -a lib/libgpg* lib/libassuan* "$sdk_rootfs_root/lib"
}

install_util_linux() {
    cd "$sdk_rootfs_prefix" || return $?
    cp -a sbin/losetup "$sdk_rootfs_root/sbin"
}

install_ldconfig() {
    cd "$TOOLCHAIN_RUNTIME_PATH" || return $?
    cp -a usr/lib/bin/ldconfig "$sdk_rootfs_root/sbin"
}

clean_misc() {
    cd "$sdk_rootfs_root" || return $?

    rm -f bin/mtd_*

    if [ "$pkg_build_type" = "Release" ]; then
        rm -rf usr/local || return $?
    fi

    find lib \( -name "*.a" -o -name "*.la" \) -delete

    remove_nss_libs || return $?

    if [ "$pkg_build_type" = "Release" ]; then
        remove_ncurses
    fi
}

install_files() {
    cp -rf "$pkg_private_dir"/rootfs/* "$sdk_rootfs_root"
}

pkg_install() {
    use_toolchain target

    create_dirs || die "create_dirs failed"
    fix_init_link || die "fix_init_link failed"
    fix_xenv_bins || die "fix_xenv_bins failed"
    clean_etc || die "clean_etc failed"
    install_keys || die "install_keys failed"
    install_gpg || die "install_gpg failed"
    install_util_linux || die "install_util_linux failed"
    install_ldconfig || die "install_ldconfig failed"
    clean_misc || die "clean_misc failed"
    remove_image_libs || die "remove_image_libs failed"
    install_files || die "install_files failed"

    p_strip "$sdk_rootfs_root" >>"$p_log" 2>&1 || die "strip failed"
}
