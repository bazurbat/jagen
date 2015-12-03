#!/bin/sh

pkg_source_dir="$jagen_src_dir/sigma-rootfs"
pkg_run_jobs=1

jagen_pkg_build() {
    use_env tools

    PATH="$SMP86XX_TOOLCHAIN_PATH/bin:$PATH"

    pkg_run cp -f config.release .config
    pkg_run make

    # contains cyclic symlinks
    rm -rf "package/udev/udev-114/test/sys"

    # cleanup while bin dir is almost empty, doing this in install script
    # confuses the shell for some reason (maybe [ [[ filenames from busybox?)
    pkg_run rm -f "$jagen_sdk_rootfs_root"/bin/*.bash

    pkg_run cd "$jagen_sdk_rootfs_root/bin"
    pkg_run rm -f setxenv unsetxenv
    pkg_run ln -fs setxenv2_mipsel setxenv2
    pkg_run ln -fs setxenv2_mipsel unsetxenv2
}

install_timezone() {
    pkg_run rm -f "$jagen_sdk_rootfs_root/etc/TZ"
    pkg_run install -m644 \
        "$TOOLCHAIN_RUNTIME_PATH/usr/share/zoneinfo/Europe/Moscow" \
        "$jagen_sdk_rootfs_root/etc/localtime"
}

install_keys() {
    pkg_run mkdir -p "$jagen_sdk_rootfs_root/lib/firmware"
    pkg_run cp -a \
        "$jagen_private_dir/keys/keyfile.gpg" \
        "$jagen_sdk_rootfs_root/lib/firmware"
}

install_gpg() {
    pkg_run cp -a \
        "$jagen_sdk_rootfs_prefix/bin/gpg" \
        "$jagen_sdk_rootfs_root/bin"
    pkg_run cp -a \
        "$jagen_sdk_rootfs_prefix"/lib/libgpg*.so* \
        "$jagen_sdk_rootfs_prefix"/lib/libassuan.so* \
        "$jagen_sdk_rootfs_root/lib"
}

install_losetup() {
    pkg_run cp -a \
        "$jagen_sdk_rootfs_prefix/sbin/losetup" \
        "$jagen_sdk_rootfs_root/sbin"
}

install_ldconfig() {
    pkg_run cp -a \
        "$TOOLCHAIN_RUNTIME_PATH/usr/lib/bin/ldconfig" \
        "$jagen_sdk_rootfs_root/sbin"
}

install_utils() {
    pkg_run cp -a \
        "$jagen_sdk_rootfs_prefix"/lib/libblkid.so* \
        "$jagen_sdk_rootfs_prefix"/lib/libmount.so* \
        "$jagen_sdk_rootfs_root/lib"
    pkg_run cp -a \
        "$jagen_sdk_rootfs_prefix/sbin/mkswap" \
        "$jagen_sdk_rootfs_prefix/sbin/swapoff" \
        "$jagen_sdk_rootfs_prefix/sbin/swapon" \
        "$jagen_sdk_rootfs_root/sbin"
}

install_files() {
    local root_dir="$jagen_sdk_rootfs_root"
    local flags_dir="$root_dir/etc/flags"

    pkg_run cp -rf "$jagen_private_dir"/rootfs/* "$root_dir"
    pkg_run mkdir -p "$flags_dir"

    if in_flags devenv; then
        pkg_run cp -rf "$jagen_private_dir"/rootfs-dev/* "$root_dir"
        rm -f "$root_dir/var/service/dropbear/down"
        touch "$flags_dir/devenv"
    fi

    if in_flags sigma_persist_logs; then
        touch "$flags_dir/persist_logs"
    fi
}

jagen_pkg_install() {
    use_toolchain target

    pkg_run cd "$jagen_sdk_rootfs_root"

    pkg_run rm -fr dev opt proc sys root tmp usr var/run
    pkg_run install -m 700 -d root
    pkg_run rm -f init linuxrc
    pkg_run ln -s /bin/busybox init

    pkg_run cd "$jagen_sdk_rootfs_root/etc"

    pkg_run rm -fr init.d network cs_rootfs_*
    pkg_run rm -f inputrc ld.so.cache mtab
    for d in up down pre-up post-down; do
        pkg_run mkdir -p network/if-${d}.d
    done

    pkg_run cd "$jagen_sdk_rootfs_root/lib"

    pkg_run rm -f libnss_compat* libnss_hesiod* libnss_nis*
    find "$jagen_sdk_rootfs_root/lib" \( -name "*.a" -o -name "*.la" \) -delete

    install_timezone
    install_keys
    install_gpg
    install_losetup
    install_ldconfig
    install_utils
    install_files

    pkg_strip_dir "$jagen_sdk_rootfs_root"
}
