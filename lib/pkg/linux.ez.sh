#!/bin/sh

p_source="git git@bitbucket.org:art-system/linux.git"
p_source_dir="$ja_src_dir/linux"
p_source_branch="ast200"

if [ "$sdk_target_board" = "ti_evm" ]; then
    defconfig="ti8168_evm_defconfig"
    uimage="uImage-evm"
elif [ "$sdk_target_board" = "ast200" ]; then
    defconfig="ast200_defconfig"
    uimage="uImage-ast200"
else
    die "Unknown target board: $sdk_target_board"
fi

pkg_build_target() {
    if [ "$pkg_build_type" = "Release" ]; then
        p_run $CROSS_MAKE $defconfig
    fi
    p_run $CROSS_MAKE uImage
    p_run $CROSS_MAKE modules
}

pkg_install_target() {
    local dest="$sdk_rootfs_dir/boot"

    p_run install -d "$dest"
    p_run install -m644 "${p_build_dir}/arch/arm/boot/uImage" "$dest/$uimage"
    p_run install -m644 "${p_build_dir}/arch/arm/boot/uImage" "/tftproot/$uimage"
    p_run install -m644 "${p_build_dir}/System.map" "$dest"

    p_run $CROSS_MAKE modules_install
}

pkg_depmod_target() {
    p_depmod
}
