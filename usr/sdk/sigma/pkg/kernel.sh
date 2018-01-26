#!/bin/sh

: ${with_kernel_config_default:=yes}
: ${with_kernel_proprietary_modules:=yes}
: ${with_kernel_extras:=yes}

export CROSS_MAKE="make ARCH=${pkg_build_arch}"
export KCFLAGS="-mhard-float -Wa,-mhard-float"

jagen_pkg_compile() {
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

    pkg_run $CROSS_MAKE -C "${ezboot_dir:?}/protector"
}

get_start_addr() {
    local NM="${pkg_toolchain_prefix}nm"
    echo 0x$($NM $1 | awk '/\<kernel_entry\>/ { print $1 }')
}

jagen_pkg_install() {
    cd linux || return $?

    pkg_run $CROSS_MAKE modules_install

    if [ $with_kernel_proprietary_modules = yes ]; then
        pkg_run cd "$pkg_source_dir/proprietary"
        pkg_run $CROSS_MAKE -C spinor modules_install
        pkg_run $CROSS_MAKE -C sd_block modules_install
    fi

    if [ $with_kernel_extras = yes ]; then
        pkg_run cd "$pkg_source_dir/extra"
        pkg_run $CROSS_MAKE modules_install
    fi

    pkg_run cd "${rootfs_root:?}/lib/modules/${pkg_build_release:?}"
    pkg_run rm -f "build" "source"
}

jagen_pkg_image() {
    local genzbf="${rootfs_prefix:?}/bin/genzbf"
    local image_dir="$jagen_target_dir/kernel-image"
    local image="$jagen_target_dir/zbimage-linux-xload"
    # for zeropad.bash
    add_PATH "${xsdk_dir:?}/xbin"

    pkg_run rm -rf "$image_dir"
    pkg_run mkdir -p "$image_dir" "$image_dir/romfs"

    pkg_run cd "$LINUX_KERNEL"
    pkg_run $CROSS_MAKE vmlinux.bin
    gzip -9c arch/mips/boot/vmlinux.bin > "$image_dir/vmlinux_gz.zbf" || return

    pkg_run cd "$image_dir"
    pkg_run bash "$jagen_private_dir/scripts/build_cpu_xload.bash" \
        vmlinux_gz $XSDK_DEFAULT_CPU_CERTID $XSDK_DEFAULT_KEY_DOMAIN
    pkg_run "$genzbf" \
        -l 0x84000000 \
        -s $(get_start_addr "$LINUX_KERNEL/vmlinux") \
        -a lzef -o romfs/vmlinux_xload.zbf \
        vmlinux_gz_${XSDK_DEFAULT_KEY_DOMAIN}.xload

    pkg_run genromfs -V MIPSLINUX_XLOAD -d romfs -f "$image"

    pkg_run "${ezboot_dir:?}/protector/zbprotector" "$image" "${image}.zbc"
    pkg_run chmod 644 "${image}.zbc"
}
