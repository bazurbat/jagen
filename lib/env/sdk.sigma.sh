#!/bin/sh

export sdk_target_board="ast100"

export sdk_ezboot_dir="$ja_src_dir/sigma-ezboot"
export sdk_kernel_dir="$ja_src_dir/sigma-kernel"
export sdk_mrua_dir="$ja_src_dir/sigma-mrua"

export sdk_rootfs_dir="$ja_src_dir/sigma-rootfs"
export sdk_rootfs_root="$sdk_rootfs_dir/build_mipsel/root"
export sdk_rootfs_prefix="$sdk_rootfs_dir/cross_rootfs"

export sdk_firmware_dir="$ja_build_dir/firmware"

export rootfs_add_e2fs_tools="yes"

toolchain_gcc=$(which mips-linux-gnu-gcc 2>/dev/null)
if [ $? = 0 ] && [ "$toolchain_gcc" ]; then
    export SMP86XX_TOOLCHAIN_PATH=$(realpath $(dirname "$toolchain_gcc")/..)
fi

#[ -d "$SMP86XX_TOOLCHAIN_PATH" ] || die "Toolchain path is not found"

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

if [ "$ja_build_type" = "Debug" ]; then
    COMPILKIND="debug glibc codesourcery hardfloat"
else
    COMPILKIND="release glibc codesourcery hardfloat"
fi
export COMPILKIND

export sdkver="3.9-nk"

case $sdkver in
    3.8)
        cpukeys="CPU_KEYS_SMP86xx_2010-02-12"
        kernel_version="2.6.22.19"
        kernel_release="${kernel_version}-35-sigma"
        ;;
    3.9)
        cpukeys="CPU_KEYS_SMP86xx_2010-02-12"
        kernel_version="2.6.22.19"
        kernel_release="${kernel_version}-35-sigma"
        ;;
    3.9-nk)
        cpukeys="CPU_KEYS_SMP86xx_2010-02-12"
        kernel_version="2.6.32"
        kernel_release="${kernel_version}.15-21-sigma"
        ;;
    3.11)
        cpukeys="CPU_KEYS_SMP86xx_2011-09-22"
        kernel_version="2.6.29"
        kernel_release="${kernel_version}.6-33-sigma"
        ;;
    3.11-ok)
        cpukeys="CPU_KEYS_SMP86xx_2011-09-22"
        kernel_version="2.6.22.19"
        kernel_release="${kernel_version}-35-sigma"
        ;;
    3.11-nk)
        cpukeys="CPU_KEYS_SMP86xx_2011-09-22"
        kernel_version="2.6.32"
        kernel_release="${kernel_version}.15-21-sigma"
        ;;
    4.0)
        cpukeys="CPU_KEYS_SMP86xx_2011-09-22"
        kernel_version="2.6.32"
        kernel_release="${kernel_version}.15-21-sigma"
        ;;
    4.0-ok)
        cpukeys="CPU_KEYS_SMP86xx_2011-09-22"
        kernel_version="2.6.29"
        kernel_release="${kernel_version}.6-33-sigma"
        ;;
esac
export cpukeys kernel_version kernel_release

export ja_xsdk_dir="$ja_build_dir/pkg/xsdk/$cpukeys"

# XSDK
export XSDK_ROOT="$ja_xsdk_dir/signed_items"
export XSDK_DEFAULT_KEY_DOMAIN=8644_ES1_prod
export XSDK_DEFAULT_ZBOOT_CERTID=0000
export XSDK_DEFAULT_CPU_CERTID=0001

if [ -d "$ja_xsdk_dir/xbin" ]; then
    PATH="$ja_xsdk_dir/xbin:$PATH"
fi

export kernel_dir="$ja_src_dir/sigma-kernel"
export kernel_modules_dir="$sdk_rootfs_root/lib/modules/$kernel_release"
export kernel_extra_modules_dir="$kernel_modules_dir/extra"

export LINUX_KERNEL="$kernel_dir/linux"
export UCLINUX_KERNEL="$LINUX_KERNEL"
