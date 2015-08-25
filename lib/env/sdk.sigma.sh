#!/bin/sh

target_prefix="/firmware"

target_system="mipsel-linux-gnu"

target_arch="mips"
target_cpu="24kf"
target_board="${target_board:-ast100}"

sdk_firmware_dir="$pkg_build_dir/firmware"

sdk_ezboot_dir="$pkg_src_dir/sigma-ezboot"
sdk_kernel_dir="$pkg_src_dir/sigma-kernel"
sdk_mrua_dir="$pkg_src_dir/sigma-mrua"

sdk_rootfs_dir="$pkg_src_dir/sigma-rootfs"
sdk_rootfs_root="$sdk_rootfs_dir/build_mipsel/root"
sdk_rootfs_prefix="$sdk_rootfs_dir/cross_rootfs"

pkg_sdk_version=${pkg_sdk_version:-311}
if [ "$pkg_sdk_version" = 311 ]; then
    pkg_flags="$pkg_flags sigma_with_monitoring"
fi

export SMP86XX_ROOTFS_PATH="$sdk_rootfs_dir"
export INSTALL_MOD_PATH="$sdk_rootfs_root"

export SMP86XX_TOOLCHAIN_PATH="$jagen_toolchain_dir"
export TOOLCHAIN_RUNTIME_PATH="$jagen_toolchain_dir/mips-linux-gnu/libc/el"

# for genzbf
add_PATH="$sdk_rootfs_prefix/bin"

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
if [ "$pkg_build_type" = "Debug" ]; then
    COMPILKIND="$COMPILKIND debug"
else
    COMPILKIND="$COMPILKIND release"
fi

if in_flags "new_kernel"; then
    # kernel_release="2.6.32.15-21-sigma"
    # kernel_release="2.6.32.15-sigma"
    # kernel_release="3.0.101-sigma+"
    kernel_release="3.4.2-sigma+"
    # kernel_release="3.4.105-sigma+"
    # kernel_release="3.5.0-sigma+"
    # kernel_release="3.6.11-sigma+"
    # kernel_release="3.7.10-sigma+"
    # kernel_release="3.10.65-sigma+"
    # kernel_release="3.18.3-sigma+"
else
    kernel_release="2.6.32.15-21-sigma"
fi
cpukeys="CPU_KEYS_SMP86xx_2010-02-12"

xsdk_dir="$pkg_build_dir/pkg/xsdk/$cpukeys"

# XSDK
export XSDK_ROOT="$xsdk_dir/signed_items"
export XSDK_DEFAULT_KEY_DOMAIN=8644_ES1_prod
export XSDK_DEFAULT_ZBOOT_CERTID=0000
export XSDK_DEFAULT_CPU_CERTID=0001

if [ -d "$xsdk_dir/xbin" ]; then
    add_PATH "$xsdk_dir/xbin"
fi

kernel_dir="$pkg_src_dir/sigma-kernel"
kernel_modules_dir="$sdk_rootfs_root/lib/modules/$kernel_release"
kernel_extra_modules_dir="$kernel_modules_dir/extra"

export LINUX_KERNEL="$kernel_dir/linux"
export UCLINUX_KERNEL="$LINUX_KERNEL"
