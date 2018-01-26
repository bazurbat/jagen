#!/bin/sh

# Do not assert non empty toolchain dir here as it is used only by install
# stage which should have the config sourced and the variable defined.
toolchain_runtime_dir="$toolchain_dir/mips-linux-gnu/libc/el"

jagen_pkg_patch() {
    pkg_patch
    pkg_run cp -f config.release .config
}

jagen_pkg_compile() {
    export SMP86XX_TOOLCHAIN_PATH="$toolchain_dir"
    add_PATH "${toolchain_dir:?}/bin"

    # Fix cp: not writing through dangling symlink '.../build_mipsel/root/init'
    pkg_run rm -f "${pkg_export_root:?}/init"

    pkg_run make
}

install_cleanup() {
    local dest="${1:?}"

    pkg_run cd "$dest"

    pkg_run rm -fr dev opt proc root tmp usr var/run
    pkg_run install -m 700 -d root

    pkg_run rm -f init
    pkg_run ln -s /bin/busybox init

    pkg_run cd "$dest/bin"

    pkg_run rm -f *.bash
    pkg_run rm -f setxenv unsetxenv
    pkg_run ln -fs setxenv2_mipsel setxenv2
    pkg_run ln -fs setxenv2_mipsel unsetxenv2

    pkg_run cd "$dest/etc"

    pkg_run rm -fr init.d network cs_rootfs_*
    pkg_run rm -f inputrc ld.so.cache mtab

    for dir in up down pre-up post-down; do
        pkg_run mkdir -p "network/if-${dir}.d"
    done

    pkg_run cd "$dest/lib"

    pkg_run rm -f libnss_compat* libnss_hesiod* libnss_nis*
    find . \( -name "*.a" -o -name "*.la" \) -delete || return
}

install_timezone() {
    local dest="${1:?}"
    : ${toolchain_runtime_dir:?}

    pkg_run rm -f "$dest/etc/TZ"
    pkg_run install -m644 \
        "$toolchain_runtime_dir/usr/share/zoneinfo/Europe/Moscow" \
        "$dest/etc/localtime"
}

install_ldconfig() {
    local dest="${1:?}"
    : ${toolchain_runtime_dir:?}

    pkg_run cp -a \
        "$toolchain_runtime_dir/usr/lib/bin/ldconfig" \
        "$dest/sbin"
}

install_gnupg() {
    local dest="${1:?}"
    : ${pkg_install_dir:?}

    pkg_run cp -a \
        "$pkg_install_dir/bin/gpg" \
        "$dest/bin"
    pkg_run cp -a \
        "$pkg_install_dir"/lib/libgpg*.so* \
        "$pkg_install_dir"/lib/libassuan.so* \
        "$dest/lib"
}

install_ntpclient() {
    local dest="${1:?}"
    : ${pkg_install_dir:?}

    pkg_run cp -a \
        "$pkg_install_dir/bin/ntpclient" \
        "$dest/bin"
}

install_util_linux() {
    local dest="${1:?}"
    : ${pkg_install_dir:?}

    pkg_run cp -a \
        "$pkg_install_dir"/lib/libblkid.so* \
        "$pkg_install_dir"/lib/libmount.so* \
        "$dest/lib"

    local programs="losetup mkswap swapoff swapon"

    for program in $programs; do
        pkg_run cp -a \
            "$pkg_install_dir/sbin/$program" \
            "$dest/sbin"
    done
}

install_utils() {
    local dest="${1:?}"
    : ${pkg_install_dir:?}

    ( pkg_run cd "$pkg_install_dir/sbin"
      pkg_run install -m 755 \
          sm smf sms mic_test spi_rw ddixcntup \
          "$dest/sbin"
    )
}

install_keys() {
    local dest="${1:?}"
    : ${jagen_private_dir:?}

    pkg_run mkdir -p "$dest/lib/firmware"
    pkg_run cp -a \
        "$jagen_private_dir/keys/keyfile.gpg" \
        "$dest/lib/firmware"
}

install_files() {
    local dest="${1:?}"
    local flags_dir="$dest/etc/flags"
    : ${jagen_private_dir:?}

    pkg_run cp -rf "$jagen_private_dir"/rootfs/* "$dest"
    pkg_run mkdir -p "$flags_dir"

    if in_flags devenv; then
        pkg_run cp -rf "$jagen_private_dir"/rootfs-dev/* "$dest"
        rm -f "$dest/var/service/dropbear/down"
        touch "$flags_dir/devenv"
    fi

    if in_flags sigma_persist_logs; then
        touch "$flags_dir/persist_logs"
    fi
}

jagen_pkg_install() {
    local dest="${pkg_export_root:?}"

    install_cleanup  "$dest" || return

    install_timezone "$dest" || return
    install_ldconfig "$dest" || return

    install_gnupg      "$dest" || return
    install_ntpclient  "$dest" || return
    install_util_linux "$dest" || return
    install_utils      "$dest" || return

    install_keys  "$dest" || return
    install_files "$dest" || return

    pkg_strip_root "$dest"
}
