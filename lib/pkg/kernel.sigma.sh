#!/bin/sh

pworkdir="$ja_srcdir/sigma-kernel"

protectordir="$ja_ezboot_dir/protector/"

use_env tools target

pkg_unpack() {
    p_cmd ln -sfT "$ja_srcdir/linux" linux
}

pkg_build() {
    p_cmd cp -f kernel-config linux/.config

    p_cmd cd linux

    p_make

    p_cmd cd "$pworkdir/proprietary"
    p_make -C spinor clean
    p_make -C spinor
    p_make -C sd_block

    p_cmd cd "$pworkdir/extra"
    p_make clean
    p_make all

    p_make -C "$protectordir"
}

pkg_install() {
    cd linux || return $?

    p_make modules_install

    p_cmd cd "$kerneldir/proprietary"
    p_make -C spinor modules_install
    p_make -C sd_block modules_install

    p_cmd cd "$kerneldir/extra"
    p_make modules_install

    p_cmd cd "$kernelmodulesdir"
    p_cmd rm -f "build" "source"
}

pkg_image() {
    p_cmd cd linux

    p_cmd cp -f "$ja_libdir/conf/initramfs_default_node_list" "usr"

    p_cmd make zbimage-linux-xload

    p_cmd cp -f arch/mips/boot/zbimage-linux-xload "$targetdir"

    p_cmd "$protectordir/zbprotector" \
        "$targetdir/zbimage-linux-xload" \
        "$targetdir/zbimage-linux-xload.zbc"
}
