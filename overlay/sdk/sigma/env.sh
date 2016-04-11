#!/bin/sh

jagen_sdk='sigma'

jagen_target_prefix="/firmware"

jagen_sdk_ezboot_dir="$jagen_src_dir/sigma-ezboot"
jagen_sdk_kernel_dir="$jagen_src_dir/sigma-kernel"
jagen_sdk_mrua_dir="$jagen_src_dir/sigma-mrua"

jagen_sdk_rootfs_dir="$jagen_src_dir/sigma-rootfs"
jagen_sdk_rootfs_root="$jagen_sdk_rootfs_dir/build_mipsel/root"
jagen_sdk_rootfs_prefix="$jagen_sdk_rootfs_dir/cross_rootfs"

jagen_sdk_initfs_dir="$jagen_sdk_rootfs_root"

jagen_sdk_staging_dir="$jagen_sdk_rootfs_dir/cross_rootfs"

export SMP86XX_ROOTFS_PATH="$jagen_sdk_rootfs_dir"
export INSTALL_MOD_PATH="$jagen_sdk_rootfs_root"

export SMP86XX_TOOLCHAIN_PATH="$jagen_toolchain_dir"
export TOOLCHAIN_RUNTIME_PATH="$jagen_toolchain_dir/mips-linux-gnu/libc/el"

# MRUA
export RMCFLAGS="-DEM86XX_CHIP=EM86XX_CHIPID_TANGO3 \
-DEM86XX_REVISION=3 \
-DEM86XX_MODE=EM86XX_MODEID_STANDALONE \
-DWITHOUT_NERO_SPU=1 \
-DWITHOUT_RMOUTPUT=1 \
-DWITH_REALVIDEO=1 \
-DWITH_XLOADED_UCODE=1 \
-DXBOOT2_SMP865X=1"

if in_flags "sigma_with_monitoring"; then
    RMCFLAGS="$RMCFLAGS -DWITH_PROC=1 -DWITH_MONITORING=1"
fi

export COMPILKIND="codesourcery glibc hardfloat"
if [ "$jagen_build_profile" = "debug" ]; then
    COMPILKIND="$COMPILKIND debug"
else
    COMPILKIND="$COMPILKIND release"
fi

jagen_kernel_release="2.6.32.15-21-sigma"
jagen_sigma_cpukeys="CPU_KEYS_SMP86xx_2010-02-12"

jagen_sigma_xsdk_dir="$jagen_build_dir/xsdk/$jagen_sigma_cpukeys"

# to find ...-config tools and other utils
add_PATH "$jagen_sdk_rootfs_prefix/bin"

# XSDK
export XSDK_ROOT="$jagen_sigma_xsdk_dir/signed_items"
export XSDK_DEFAULT_KEY_DOMAIN=8644_ES1_prod
export XSDK_DEFAULT_ZBOOT_CERTID=0000
export XSDK_DEFAULT_CPU_CERTID=0001

if [ -d "$jagen_sigma_xsdk_dir/xbin" ]; then
    add_PATH "$jagen_sigma_xsdk_dir/xbin"
fi

jagen_kernel_dir="$jagen_src_dir/sigma-kernel"
jagen_kernel_modules_dir="$jagen_sdk_rootfs_root/lib/modules/$jagen_kernel_release"
jagen_kernel_extra_modules_dir="$jagen_kernel_modules_dir/extra"

export LINUX_KERNEL="$jagen_src_dir/linux"
export UCLINUX_KERNEL="$LINUX_KERNEL"

pkg_using_sigma_sdk() {
    local S="$jagen_FS" A=
    A="$A$S-DSIGMA_SDK_DIR=$jagen_src_dir/sigma-mrua"
    A="$A$S-DSIGMA_ROOTFS_DIR=$jagen_src_dir/sigma-rootfs"
    printf '%s' "$A"
}
