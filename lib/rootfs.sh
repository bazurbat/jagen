#!/bin/sh

jagen_rootfs_init() {
    local dir="${1:?}"

    pkg_run install -d -m 755 \
        "$dir/bin"   \
        "$dir/dev"   \
        "$dir/etc"   \
        "$dir/home"  \
        "$dir/lib"   \
        "$dir/mnt"   \
        "$dir/proc"  \
        "$dir/root"  \
        "$dir/run"   \
        "$dir/sbin"  \
        "$dir/share" \
        "$dir/sys"   \
        "$dir/tmp"   \
        "$dir/usr"   \
        "$dir/var"
    pkg_run chmod 0700 "$dir/root"
    pkg_run chmod 1777 "$dir/tmp"

    pkg_run install -d -m 755 "$dir/run/lock"
    pkg_run install -d -m 755 "$dir/var/log"

    pkg_run ln -sf ../run "$dir/var"
    pkg_run ln -sf ../run/lock "$dir/var"

    # backward compatibility with Sigma firmware
    pkg_run ln -snf var "$dir/settings"
}

jagen_rootfs_install_hostname() {
    echo "${jagen_target_board:?}" > etc/hostname
}

jagen_rootfs_fix_mtab() {
    pkg_run ln -snf /proc/mounts etc/mtab
}
