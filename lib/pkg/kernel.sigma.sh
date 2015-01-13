#!/bin/sh

p_source="git git@bitbucket.org:art-system/sigma-kernel.git"
p_source_dir="$pkg_src_dir/sigma-kernel"

protectordir="$sdk_ezboot_dir/protector/"

use_env tools
use_toolchain target

pkg_build() {
    p_run ln -sfT "$pkg_src_dir/linux" linux

    p_run cp -f kernel-config linux/.config

    p_run cd linux

    p_run $CROSS_MAKE

    p_run cd "$p_source_dir/proprietary"
    p_run $CROSS_MAKE -C spinor clean
    p_run $CROSS_MAKE -C spinor
    p_run $CROSS_MAKE -C sd_block

    p_run cd "$p_source_dir/extra"
    p_run $CROSS_MAKE clean
    p_run $CROSS_MAKE all

    p_run $CROSS_MAKE -C "$protectordir"
}

pkg_install() {
    cd linux || return $?

    p_run $CROSS_MAKE modules_install

    p_run cd "$kernel_dir/proprietary"
    p_run $CROSS_MAKE -C spinor modules_install
    p_run $CROSS_MAKE -C sd_block modules_install

    p_run cd "$kernel_dir/extra"
    p_run $CROSS_MAKE modules_install

    p_run cd "$kernel_modules_dir"
    p_run rm -f "build" "source"
}

pkg_image() {
    p_run cd linux

    p_run $CROSS_MAKE zbimage-linux-xload

    p_run cp -f arch/mips/boot/zbimage-linux-xload "$target_dir"

    p_run "$protectordir/zbprotector" \
        "$target_dir/zbimage-linux-xload" \
        "$target_dir/zbimage-linux-xload.zbc"
}
