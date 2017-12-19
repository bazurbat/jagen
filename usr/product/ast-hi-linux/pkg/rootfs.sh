#!/bin/sh

require rootfs
require toolchain

install_sdk() {
    local pub_dir="$jagen_sdk_dir/pub"

    if [ -d "$pub_dir/rootfs" ]; then
        pkg_run rsync -rtl "$jagen_sdk_dir/pub/rootfs/" "."
    fi
    pkg_run rsync -rtl "$pub_dir/lib/share/" "lib"
    pkg_run rsync -rtl --delete "$pub_dir/kmod/" "kmod"
}

jagen_pkg_compile() {
    jagen_rootfs_init .
    toolchain_install_runtime
    toolchain_install_ldconfig
}

jagen_pkg_install() {
    install_sdk

    pkg_run rsync -t "$jagen_private_dir/lib/libHA.AUDIO.PCM.decode.so" "lib"
    pkg_run rsync -aFF "$pkg_source_dir/hisi/" .
    pkg_run chmod 0700 root/.ssh

    pkg_run mkdir -p lib/firmware
    pkg_run rsync -t "$jagen_private_dir/c6747/2McASP_BEST.bin" \
        "lib/firmware/c6747.bin"
    pkg_run rsync -t "$jagen_private_dir/keys/keyfile.gpg" \
        "lib/firmware"

    jagen_rootfs_install_hostname
    jagen_rootfs_fix_mtab

    if pkg_is_release; then
        _jagen src status > heads || return
    fi

    if in_flags devenv; then
        pkg_run touch "$pkg_install_dir/var/service/connman/down"
        pkg_run rm -f "$pkg_install_dir/var/service/dropbear/down"
    else
        pkg_run rm -f "$pkg_install_dir/var/service/connman/down"
        pkg_run touch "$pkg_install_dir/var/service/dropbear/down"
    fi
}
