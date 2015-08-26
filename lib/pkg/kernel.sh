#!/bin/sh

use_toolchain target

: ${with_kernel_config_default:=yes}
: ${with_kernel_proprietary_modules:=yes}
: ${with_kernel_extras:=yes}

export CROSS_COMPILE="${toolchain_bin_dir}/${target_system}-"
export CROSS_MAKE="make ARCH=${target_arch}"
export KCFLAGS="-mhard-float -Wa,-mhard-float"

protectordir="$sdk_ezboot_dir/protector/"

pkg_build() {
    p_run ln -sfT "$pkg_src_dir/linux" linux

    if [ $with_kernel_config_default = yes ]; then
        p_run cp -f kernel-config linux/.config
    fi

    p_run cd linux

    p_run $CROSS_MAKE

    if [ $with_kernel_proprietary_modules = yes ]; then
        p_run cd "$p_source_dir/proprietary"
        p_run $CROSS_MAKE -C spinor clean
        p_run $CROSS_MAKE -C spinor
        p_run $CROSS_MAKE -C sd_block
    fi

    if [ $with_kernel_extras = yes ]; then
        p_run cd "$p_source_dir/extra"
        p_run $CROSS_MAKE clean
        p_run $CROSS_MAKE all
    fi

    p_run $CROSS_MAKE -C "$protectordir"
}

pkg_install() {
    cd linux || return $?

    p_run $CROSS_MAKE modules_install

    if [ $with_kernel_proprietary_modules = yes ]; then
        p_run cd "$kernel_dir/proprietary"
        p_run $CROSS_MAKE -C spinor modules_install
        p_run $CROSS_MAKE -C sd_block modules_install
    fi

    if [ $with_kernel_extras = yes ]; then
        p_run cd "$kernel_dir/extra"
        p_run $CROSS_MAKE modules_install
    fi

    p_run cd "$kernel_modules_dir"
    p_run rm -f "build" "source"
}

get_start_addr() {
    local NM="${toolchain_bin_dir}/${target_system}-nm"
    echo 0x$($NM $1 | awk '/\<kernel_entry\>/ { print $1 }')
}

pkg_image() {
    add_PATH "$sdk_rootfs_prefix/bin"

    local tmpdir="$target_dir/kernel-image"
    p_clean_dir "$tmpdir"

    p_run cd linux
    p_run $CROSS_MAKE vmlinux.bin
    gzip -9cnf arch/mips/boot/vmlinux.bin > "$tmpdir/vmlinux_gz.zbf" || exit

    p_run cd "$tmpdir"
    p_run bash "$pkg_private_dir/scripts/build_cpu_xload.bash" \
        vmlinux_gz $XSDK_DEFAULT_CPU_CERTID $XSDK_DEFAULT_KEY_DOMAIN
    p_run genzbf \
        -l 0x84000000 \
        -s $(get_start_addr "$LINUX_KERNEL/vmlinux") \
        -a lzef -o vmlinux_xload.zbf \
        vmlinux_gz_${XSDK_DEFAULT_KEY_DOMAIN}.xload

    p_clean_dir romfs
    p_run cp vmlinux_xload.zbf romfs
    p_run genromfs -V MIPSLINUX_XLOAD -d romfs \
        -f "$target_dir/zbimage-linux-xload"

    p_run "$protectordir/zbprotector" \
        "$target_dir/zbimage-linux-xload" \
        "$target_dir/zbimage-linux-xload.zbc"
}
