#!/bin/sh

export target_dir="$pkg_build_dir/target"
export target_prefix="/firmware"

export target_arch="mips"
export target_cpu="24kf"
export target_system="mipsel-linux-gnu"

export target_bin_dir="$target_dir/bin"

export sdk_target_board="ast100"

export sdk_ezboot_dir="$pkg_src_dir/sigma-ezboot"
export sdk_kernel_dir="$pkg_src_dir/sigma-kernel"
export sdk_mrua_dir="$pkg_src_dir/sigma-mrua"

export sdk_rootfs_dir="$pkg_src_dir/sigma-rootfs"
export sdk_rootfs_root="$sdk_rootfs_dir/build_mipsel/root"
export sdk_rootfs_prefix="$sdk_rootfs_dir/cross_rootfs"

export sdk_firmware_dir="$pkg_build_dir/firmware"

toolchain_gcc=$(which mips-linux-gnu-gcc 2>/dev/null)
if [ $? = 0 ] && [ "$toolchain_gcc" ]; then
    export SMP86XX_TOOLCHAIN_PATH=$(realpath $(dirname "$toolchain_gcc")/..)
fi

export TOOLCHAIN_RUNTIME_PATH="${SMP86XX_TOOLCHAIN_PATH}/mips-linux-gnu/libc/el"

export RMCFLAGS="\
-DEM86XX_CHIP=EM86XX_CHIPID_TANGO3 \
-DEM86XX_MODE=EM86XX_MODEID_STANDALONE \
-DEM86XX_REVISION=3 \
-DRMCHIP_ID=RMCHIP_ID_SMP8652 \
-DRMCHIP_REVISION=3 \
-DWITHOUT_NERO_SPU=1 \
-DWITHOUT_RMOUTPUT=1 \
-DWITH_PROD=1 \
-DWITH_REALVIDEO=1 \
-DWITH_XLOADED_UCODE=1 \
-DXBOOT2_SMP865X=1 \
-DXBOOT2_SMP8670=1 \
"

if [ "$pkg_build_type" = "Debug" ]; then
    COMPILKIND="debug glibc codesourcery hardfloat"
else
    COMPILKIND="release glibc codesourcery hardfloat"
fi
export COMPILKIND

if in_flags "new_kernel"; then
    kernel_release="2.6.32.15-sigma"
else
    kernel_release="2.6.32.15-sigma"
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
    p_path_prepend "$xsdk_dir/xbin"
fi

export kernel_dir="$pkg_src_dir/sigma-kernel"
export kernel_modules_dir="$sdk_rootfs_root/lib/modules/$kernel_release"
export kernel_extra_modules_dir="$kernel_modules_dir/extra"

export LINUX_KERNEL="$kernel_dir/linux"
export UCLINUX_KERNEL="$LINUX_KERNEL"
