#!/bin/sh

use_toolchain target

: ${with_kernel_config_default:=yes}
: ${with_kernel_proprietary_modules:=yes}
: ${with_kernel_extras:=yes}

export CROSS_COMPILE="${jagen_target_toolchain_dir}/bin/${jagen_target_system}-"
export CROSS_MAKE="make ARCH=${jagen_target_arch}"
export KCFLAGS="-mhard-float -Wa,-mhard-float"

protectordir="$jagen_sdk_ezboot_dir/protector/"

jagen_pkg_build() {
    pkg_run ln -sfT "$jagen_src_dir/linux" linux

    if [ $with_kernel_config_default = yes ]; then
        pkg_run cp -f kernel-config linux/.config
    fi

    pkg_run cd linux

    pkg_run $CROSS_MAKE

    if [ $with_kernel_proprietary_modules = yes ]; then
        pkg_run cd "$pkg_source_dir/proprietary"
        pkg_run $CROSS_MAKE -C spinor clean
        pkg_run $CROSS_MAKE -C spinor
        pkg_run $CROSS_MAKE -C sd_block
    fi

    if [ $with_kernel_extras = yes ]; then
        pkg_run cd "$pkg_source_dir/extra"
        pkg_run $CROSS_MAKE clean
        pkg_run $CROSS_MAKE all
    fi

    pkg_run $CROSS_MAKE -C "$protectordir"
}

jagen_pkg_install() {
    cd linux || return $?

    pkg_run $CROSS_MAKE modules_install

    if [ $with_kernel_proprietary_modules = yes ]; then
        pkg_run cd "$jagen_kernel_dir/proprietary"
        pkg_run $CROSS_MAKE -C spinor modules_install
        pkg_run $CROSS_MAKE -C sd_block modules_install
    fi

    if [ $with_kernel_extras = yes ]; then
        pkg_run cd "$jagen_kernel_dir/extra"
        pkg_run $CROSS_MAKE modules_install
    fi

    pkg_run cd "$jagen_kernel_modules_dir"
    pkg_run rm -f "build" "source"
}

get_start_addr() {
    local NM="${jagen_target_toolchain_dir}/bin/${jagen_target_system}-nm"
    echo 0x$($NM $1 | awk '/\<kernel_entry\>/ { print $1 }')
}

jagen_pkg_image() {
    add_PATH "$jagen_sdk_rootfs_prefix/bin"

    local tmpdir="$jagen_target_dir/kernel-image"
    pkg_clean_dir "$tmpdir"

    pkg_run cd "$LINUX_KERNEL"
    pkg_run $CROSS_MAKE vmlinux.bin
    gzip -9cnf arch/mips/boot/vmlinux.bin > "$tmpdir/vmlinux_gz.zbf" || exit

    pkg_run cd "$tmpdir"
    pkg_run bash "$jagen_private_dir/scripts/build_cpu_xload.bash" \
        vmlinux_gz $XSDK_DEFAULT_CPU_CERTID $XSDK_DEFAULT_KEY_DOMAIN
    pkg_run genzbf \
        -l 0x84000000 \
        -s $(get_start_addr "$LINUX_KERNEL/vmlinux") \
        -a lzef -o vmlinux_xload.zbf \
        vmlinux_gz_${XSDK_DEFAULT_KEY_DOMAIN}.xload

    pkg_clean_dir romfs
    pkg_run cp vmlinux_xload.zbf romfs
    pkg_run genromfs -V MIPSLINUX_XLOAD -d romfs \
        -f "$jagen_target_dir/zbimage-linux-xload"

    pkg_run "$protectordir/zbprotector" \
        "$jagen_target_dir/zbimage-linux-xload" \
        "$jagen_target_dir/zbimage-linux-xload.zbc"
}
