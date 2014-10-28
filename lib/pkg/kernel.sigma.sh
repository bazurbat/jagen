#!/bin/sh

pworkdir="$ja_srcdir/sigma-kernel"

protectordir="$ja_ezboot_dir/protector/"

use_env tools target

pkg_unpack() {
    p_run ln -sfT "$ja_srcdir/linux" linux
}

pkg_build() {
    p_run cp -f kernel-config linux/.config

    p_run cd linux

    p_run make

    p_run cd "$pworkdir/proprietary"
    p_run make -C spinor clean
    p_run make -C spinor
    p_run make -C sd_block

    p_run cd "$pworkdir/extra"
    p_run make clean
    p_run make all

    p_run make -C "$protectordir"
}

pkg_install() {
    cd linux || return $?

    p_run make modules_install

    p_run cd "$kerneldir/proprietary"
    p_run make -C spinor modules_install
    p_run make -C sd_block modules_install

    p_run cd "$kerneldir/extra"
    p_run make modules_install

    p_run cd "$kernelmodulesdir"
    p_run rm -f "build" "source"
}

pkg_image() {
    p_run cd linux

    p_run cp -f "$ja_srcdir/misc/cfg/initramfs_default_node_list" "usr"

    p_run make zbimage-linux-xload

    p_run cp -f arch/mips/boot/zbimage-linux-xload "$targetdir"

    p_run "$protectordir/zbprotector" \
        "$targetdir/zbimage-linux-xload" \
        "$targetdir/zbimage-linux-xload.zbc"
}
