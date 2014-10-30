#!/bin/sh

p_work_dir="$sdk_rootfs_dir"

pkg_unpack() {
    local A=$(ls -1 $EZSDK/filesystem/ezsdk-dm816x-evm-rootfs.tar.gz)
    local me=$(whoami)

    sudo -n rm -rf "$p_work_dir" || return $?
    mkdir "$p_work_dir" || return $?
    sudo -n tar -C "$p_work_dir" -xf "$A" || return $?
    sudo -n chown "$me" "$p_work_dir" || return $?

    cd "$p_work_dir" || return $?

    sudo -n chown -R "$me" bin boot etc home lib media mnt opt sbin usr || return $?

    # remove psplash
    mv etc/rcS.d/S01psplash etc/rcS.d/K01psplash || return $?
    rm etc/rc0.d/K20psplash || return $?
}

pkg_prepare() {
    local rules="$EZSDK/Rules.make"
    local platform=$(grep -e "^PLATFORM=" "$rules" | cut -d= -f2)

    sed -i "s=^EXEC_DIR\=.*$=EXEC_DIR\=$p_work_dir/home/root/$platform=g" "$rules" || return $?
    sed -i "s=^DESTDIR\=.*$=DESTDIR\=$p_work_dir=g" "$rules" || return $?
}
