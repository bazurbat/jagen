#!/bin/sh

install_sdk() {
    local pub_dir="$jagen_sdk_dir/pub"

    if [ -d "$pub_dir/rootfs" ]; then
        pkg_run rsync -rtl "$jagen_sdk_dir/pub/rootfs/" "."
    fi
    pkg_run rsync -rtl "$pub_dir/lib/share/" "lib"
    pkg_run rsync -rtl --delete "$pub_dir/kmod/" "kmod"
}

jagen_pkg_install() {
    require rootfs

    jagen_rootfs_init .
    jagen_toolchain_install_runtime .

    install_sdk

    pkg_run rsync -t "$jagen_private_dir/lib/libHA.AUDIO.PCM.decode.so" "lib"
    pkg_run rsync -aFF "$pkg_source_dir/hisi/" .
    pkg_run chmod 0700 root/.ssh

    jagen_rootfs_install_hostname
    jagen_rootfs_fix_mtab

    rm -f "$pkg_source_dir/var/service/dropbear/down"

    # if pkg_is_release; then
    #     _jagen src status > heads || return
    # fi
}
