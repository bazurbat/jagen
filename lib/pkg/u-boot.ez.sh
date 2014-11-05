#!/bin/sh

p_source="git git@bitbucket.org:art-system/u-boot.git"
p_source_dir="$ja_src_dir/$p_name"

if [ "$sdk_target_board" = "ti_evm" ]; then
    config="ti8168_evm_config_sd"
    config_min="ti8168_evm_min_sd"
    boot_scipt="boot_nfs_evm.txt"
elif [ "$sdk_target_board" = "ast200" ]; then
    config="ast200_sd"
    config_min="ast200_sd_min"
    boot_scipt="boot_nfs.txt"
else
    die "Unknown target board: $sdk_target_board"
fi

mkimage="./tools/mkimage -A arm -O linux -T script -C none -n TI_script -d"

pkg_build_min() {
    local dest="$sdk_rootfs_dir/boot"

    use_env target

    p_run $CROSS_MAKE distclean
    p_run $CROSS_MAKE $config_min
    p_run $CROSS_MAKE u-boot.ti

    p_run install -d "$dest"
    p_run install -m644 u-boot.min.sd "$dest/MLO"
}

pkg_build_target() {
    local dest="$sdk_rootfs_dir/boot"

    p_run $CROSS_MAKE distclean
    p_run $CROSS_MAKE $config
    p_run $CROSS_MAKE u-boot.ti

    p_run install -d "$dest"
    p_run install -m644 u-boot.bin "$dest"
}

pkg_mkimage_target() {
    p_run $mkimage "$ja_files_dir/boot/$boot_scipt" "$sdk_rootfs_dir/boot/boot.scr"
}
