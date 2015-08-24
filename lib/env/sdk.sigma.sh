#!/bin/sh

export target_dir="$pkg_build_dir/target"
export target_prefix="/firmware"

export target_arch="mips"
export target_cpu="24kf"
export target_system="mipsel-linux-gnu"

export target_bin_dir="$target_dir/bin"

export target_board="ast100"

export sdk_ezboot_dir="$pkg_src_dir/sigma-ezboot"
export sdk_kernel_dir="$pkg_src_dir/sigma-kernel"
export sdk_mrua_dir="$pkg_src_dir/sigma-mrua"

export sdk_rootfs_dir="$pkg_src_dir/sigma-rootfs"
export sdk_rootfs_root="$sdk_rootfs_dir/build_mipsel/root"
export sdk_rootfs_prefix="$sdk_rootfs_dir/cross_rootfs"

export sdk_firmware_dir="$pkg_build_dir/firmware"

export pkg_sdk_version=${pkg_sdk_version:-311}
if [ "$pkg_sdk_version" = 311 ]; then
    pkg_flags="$pkg_flags sigma_with_monitoring"
fi

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
export kernel_release
export cpukeys="CPU_KEYS_SMP86xx_2010-02-12"

xsdk_dir="$pkg_build_dir/pkg/xsdk/$cpukeys"

# XSDK
export XSDK_ROOT="$xsdk_dir/signed_items"
export XSDK_DEFAULT_KEY_DOMAIN=8644_ES1_prod
export XSDK_DEFAULT_ZBOOT_CERTID=0000
export XSDK_DEFAULT_CPU_CERTID=0001

if [ -d "$xsdk_dir/xbin" ]; then
    add_PATH "$xsdk_dir/xbin"
fi

export kernel_dir="$pkg_src_dir/sigma-kernel"
export kernel_modules_dir="$sdk_rootfs_root/lib/modules/$kernel_release"
export kernel_extra_modules_dir="$kernel_modules_dir/extra"

export LINUX_KERNEL="$kernel_dir/linux"
export UCLINUX_KERNEL="$LINUX_KERNEL"
