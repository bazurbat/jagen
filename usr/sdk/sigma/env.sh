#!/bin/sh

jagen_sdk='sigma'

jagen_sdk_rootfs_dir="$jagen_src_dir/sigma-rootfs"
jagen_sdk_rootfs_root="$jagen_sdk_rootfs_dir/build_mipsel/root"

export SMP86XX_ROOTFS_PATH="$jagen_sdk_rootfs_dir"
export INSTALL_MOD_PATH="$jagen_sdk_rootfs_root"

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

jagen_kernel_dir="$jagen_src_dir/sigma-kernel"
jagen_kernel_modules_dir="$jagen_sdk_rootfs_root/lib/modules/$jagen_kernel_release"
jagen_kernel_extra_modules_dir="$jagen_kernel_modules_dir/extra"

export LINUX_KERNEL="$jagen_src_dir/linux"
export UCLINUX_KERNEL="$LINUX_KERNEL"

KERNEL_SRC="$LINUX_KERNEL"

pkg_using_sigma_sdk() {
    local S="$jagen_FS" A=
    A="$A$S-DSIGMA_SDK_DIR=$jagen_src_dir/sigma-mrua"
    A="$A$S-DSIGMA_ROOTFS_DIR=$jagen_src_dir/sigma-rootfs"
    printf '%s' "$A"
}
