#!/bin/sh

p_source_dir="$pkg_src_dir/sigma-rootfs"
p_jobs=1

[ -d "$SMP86XX_TOOLCHAIN_PATH" ] ||
    die "SMP86XX_TOOLCHAIN_PATH ($SMP86XX_TOOLCHAIN_PATH) is not found"

pkg_patch() {
    [ -d dl ] || p_src_copy "$pkg_dist_dir/dl" "$p_build_dir/dl"
}

pkg_build() {
    use_env tools

    p_run cp -f config.release .config
    p_run make

    # contains cyclic symlinks
    rm -rf "package/udev/udev-114/test/sys"

    # cleanup while bin dir is almost empty, doing this in install script
    # confuses the shell for some reason (maybe [ [[ filenames from busybox?)
    p_run rm -f "$sdk_rootfs_root"/bin/*.bash

    p_run cd "$sdk_rootfs_root/bin"
    p_run rm -f setxenv unsetxenv
    p_run ln -fs setxenv2_mipsel setxenv2
    p_run ln -fs setxenv2_mipsel unsetxenv2
}

install_alsa() {
    p_run cp -a \
        "$sdk_rootfs_prefix/bin/alsa"* \
        "$sdk_rootfs_prefix/bin/amixer" \
        "$sdk_rootfs_prefix/bin/aplay" \
        "$sdk_rootfs_prefix/bin/arecord" \
        "$sdk_rootfs_root/bin"
    p_run cp -a \
        "$sdk_rootfs_prefix/sbin/alsactl" \
        "$sdk_rootfs_root/sbin"
    p_run cp -a \
        "$sdk_rootfs_prefix/lib/alsa-lib" \
        "$sdk_rootfs_prefix/lib/libasound"* \
        "$sdk_rootfs_root/lib"
    p_run cp -a \
        "$sdk_rootfs_prefix/share/alsa" \
        "$sdk_rootfs_root/share/alsa"
}

install_timezone() {
    p_run rm -f "$sdk_rootfs_root/etc/TZ"
    p_run install -m644 \
        "$TOOLCHAIN_RUNTIME_PATH/usr/share/zoneinfo/Europe/Moscow" \
        "$sdk_rootfs_root/etc/localtime"
}

install_keys() {
    p_run mkdir -p "$sdk_rootfs_root/lib/firmware"
    p_run cp -a \
        "$pkg_private_dir/keys/keyfile.gpg" \
        "$sdk_rootfs_root/lib/firmware"
}

install_gpg() {
    p_run cp -a \
        "$sdk_rootfs_prefix/bin/gpg" \
        "$sdk_rootfs_root/bin"
    p_run cp -a \
        "$sdk_rootfs_prefix"/lib/libgpg* \
        "$sdk_rootfs_prefix"/lib/libassuan* \
        "$sdk_rootfs_root/lib"
}

install_losetup() {
    p_run cp -a \
        "$sdk_rootfs_prefix/sbin/losetup" \
        "$sdk_rootfs_root/sbin"
}

install_ldconfig() {
    p_run cp -a \
        "$TOOLCHAIN_RUNTIME_PATH/usr/lib/bin/ldconfig" \
        "$sdk_rootfs_root/sbin"
}

install_files() {
    p_run cp -rf "$pkg_private_dir"/rootfs/* "$sdk_rootfs_root"
}

pkg_install() {
    use_toolchain target

    p_run cd "$sdk_rootfs_root"

    p_run rm -fr dev opt proc sys root tmp usr var/run
    p_run install -m 700 -d root
    p_run rm -f init linuxrc
    p_run ln -s /bin/busybox init

    p_run cd "$sdk_rootfs_root/etc"

    p_run rm -fr init.d network cs_rootfs_*
    p_run rm -f inputrc ld.so.cache mtab
    for d in up down pre-up post-down; do
        p_run mkdir -p network/if-${d}.d
    done

    p_run cd "$sdk_rootfs_root/lib"

    p_run rm -f libnss_compat* libnss_hesiod* libnss_nis*
    find "$sdk_rootfs_root/lib" \( -name "*.a" -o -name "*.la" \) -delete

    if in_flags with_alsa; then
        install_alsa
    fi
    install_timezone
    install_keys
    install_gpg
    install_losetup
    install_ldconfig
    install_files

    p_strip "$sdk_rootfs_root"
}
