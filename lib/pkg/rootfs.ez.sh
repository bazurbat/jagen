#!/bin/sh

p_work_dir="$sdk_rootfs_dir"

pkg_clean() {
    sudo -n rm -rf "$p_work_dir" || return $?
    sudo -n mkdir -p "$p_work_dir" || return $?
}

pkg_unpack() {
    local A=$(ls -1 $EZSDK/filesystem/ezsdk-dm816x-evm-rootfs.tar.gz)
    local me=$(whoami)

    sudo -n tar -C "$p_work_dir" -xf "$A" || return $?
    sudo -n chown "$me" "$p_work_dir" || return $?

    cd "$p_work_dir" || return $?

    sudo -n chown -R "$me" bin boot etc home lib media mnt opt sbin usr || return $?

    # remove psplash
    mv etc/rcS.d/S01psplash etc/rcS.d/K01psplash || return $?
    rm etc/rc0.d/K20psplash || return $?
}

pkg_patch() {
    local platform=$(grep -e "^PLATFORM=" "$sdk_rules" | cut -d= -f2)

    p_run sed -ri \
        "s|(\s*DESTDIR)=\S+|\1=$p_work_dir|g" \
        "$sdk_rules"
    p_run sed -ri \
        "s|(\s*EXEC_DIR)=\S+|\1=$p_work_dir/home/root/$platform|g" \
        "$sdk_rules"
    p_run sed -ri \
        "s|(\s*LINUXKERNEL_INSTALL_DIR)=\S+|\1=$LINUX_KERNEL|g" \
        "$sdk_rules"
}
