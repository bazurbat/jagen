#!/bin/sh

require rootfs

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

    jagen_rootfs_install_hostname
    jagen_rootfs_fix_mtab

    if pkg_is_release; then
        pkg_strip_root "$pkg_install_dir"
        _jagen src status > heads || return
    else
        pkg_run touch "$pkg_install_dir/var/service/connman/down"
        pkg_run rm -f "$pkg_install_dir/var/service/dropbear/down"
    fi
}

jagen_pkg_deploy() {
    local dst="$jagen_rootfs_deploy_dir"

    [ -d "$dst" ] || return 0

    # HACK: this facilitates deploying on a booted rootfs but will hang if
    # there is password prompt for sudo, works only in special docker image
 
    pkg_run sudo rsync -a "$pkg_install_dir/" "$dst"

    pkg_run sudo chown -R 0:0 "$dst/root"

    pkg_run sudo chmod 0700 "$dst/root"

    pkg_run sudo chmod 0700 "$dst/root/.ssh"
    pkg_run sudo chmod 0400 "$dst/root/.ssh/authorized_keys"
}
