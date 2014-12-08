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
    p_run rm -rf dev opt proc sys root tmp usr var/run
    p_run install -m 700 -d root
}

fix_init_link() {
    p_run cd "$sdk_rootfs_root"
    p_run rm -f init linuxrc
    p_run ln -s /bin/busybox init
}

fix_xenv_bins() {
    p_run cd "$sdk_rootfs_root/bin"
    p_run rm -f setxenv setxenv.bash unsetxenv
    p_run ln -sf setxenv2_mipsel setxenv2
    p_run ln -sf setxenv2_mipsel unsetxenv2
}

clean_etc() {
    p_run cd "$sdk_rootfs_root/etc"
    p_run rm -rf init.d rc.d network cs_rootfs_*
    p_run rm -f ld.so.cache mtab
    for d in up down pre-up post-down; do
        p_run mkdir -p network/if-${d}.d
    done

    p_run rm -f "$sdk_rootfs_root/etc/TZ"
    p_run install -m644 \
        "$TOOLCHAIN_RUNTIME_PATH/usr/share/zoneinfo/Europe/Moscow" \
        "$sdk_rootfs_root/etc/localtime"
}

remove_nss_libs() {
    p_run cd "$sdk_rootfs_root/lib"
    p_run rm -f libnss_compat* libnss_hesiod* libnss_nis*
}

remove_ncurses() {
    p_run cd "$sdk_rootfs_root"
    p_run rm -f etc/inputrc
    p_run rm -rf usr/share/terminfo
    p_run cd "$sdk_rootfs_root/lib"
    p_run rm -f libmenu* libpanel* libform* libncurses*
}

remove_image_libs() {
    p_run cd "$sdk_rootfs_root/lib"
    p_run rm -f libjpeg*
    p_run rm -f libpng*
    p_run rm -f libtiff*
    p_run rm -f libungif*
}

install_keys() {
    p_run mkdir -p "$sdk_rootfs_root/lib/firmware"
    p_run cp -a \
        "$pkg_private_dir/keys/keyfile.gpg" \
        "$sdk_rootfs_root/lib/firmware"
}

install_gpg() {
    p_run cd "$sdk_rootfs_prefix"
    p_run cp -a bin/gpg "$sdk_rootfs_root/bin"
    p_run cp -a lib/libgpg* lib/libassuan* "$sdk_rootfs_root/lib"
}

install_util_linux() {
    p_run cd "$sdk_rootfs_prefix"
    p_run cp -a sbin/losetup "$sdk_rootfs_root/sbin"
}

install_ldconfig() {
    p_run cd "$TOOLCHAIN_RUNTIME_PATH"
    p_run cp -a usr/lib/bin/ldconfig "$sdk_rootfs_root/sbin"
}

clean_misc() {
    p_run cd "$sdk_rootfs_root"

    p_run rm -f bin/mtd_*

    if [ "$pkg_build_type" = "Release" ]; then
        p_run rm -rf usr/local
    fi

    p_run find lib \( -name "*.a" -o -name "*.la" \) -delete

    remove_nss_libs || return $?

    if [ "$pkg_build_type" = "Release" ]; then
        remove_ncurses
    fi
}

install_files() {
    p_run cp -rf "$pkg_private_dir"/rootfs/* "$sdk_rootfs_root"
}

pkg_install() {
    use_toolchain target

    create_dirs
    fix_init_link
    fix_xenv_bins
    clean_etc
    install_keys
    install_gpg
    install_util_linux
    install_ldconfig
    clean_misc
    remove_image_libs
    install_files

    p_strip "$sdk_rootfs_root"
}
