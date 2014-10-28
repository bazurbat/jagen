#!/bin/sh

export ja_ezboot_dir="$ja_srcdir/sigma-ezboot"
export ja_kernel_dir="$ja_srcdir/sigma-kernel"
export ja_mrua_dir="$ja_srcdir/sigma-mrua"

export sdk_rootfsdir="$ja_srcdir/sigma-rootfs"
export sdk_rootfs_root="$sdk_rootfsdir/build_mipsel/root"
export sdk_rootfs_prefix="$sdk_rootfsdir/cross_rootfs"

export rootfs_add_e2fs_tools="yes"

export firmwaredir="$ja_builddir/firmware"

export ja_files_dir="$ja_srcdir/misc"

toolchain_gcc=$(which mips-linux-gnu-gcc 2>/dev/null)
if [ $? = 0 ] && [ "$toolchain_gcc" ]; then
    export SMP86XX_TOOLCHAIN_PATH=$(realpath $(dirname "$toolchain_gcc")/..)
fi

[ -d "$SMP86XX_TOOLCHAIN_PATH" ] || die "Toolchain path is not found"

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

if [ "$ja_buildtype" = "Debug" ]; then
    COMPILKIND="debug glibc codesourcery hardfloat"
else
    COMPILKIND="release glibc codesourcery hardfloat"
fi
export COMPILKIND

export sdkver="3.9-nk"

case $sdkver in
    3.8)
        cpukeys="CPU_KEYS_SMP86xx_2010-02-12"
        kernelversion="2.6.22.19"
        kernelrelease="${kernelversion}-35-sigma"
        ;;
    3.9)
        cpukeys="CPU_KEYS_SMP86xx_2010-02-12"
        kernelversion="2.6.22.19"
        kernelrelease="${kernelversion}-35-sigma"
        ;;
    3.9-nk)
        cpukeys="CPU_KEYS_SMP86xx_2010-02-12"
        kernelversion="2.6.32"
        kernelrelease="${kernelversion}.15-21-sigma"
        ;;
    3.11)
        cpukeys="CPU_KEYS_SMP86xx_2011-09-22"
        kernelversion="2.6.29"
        kernelrelease="${kernelversion}.6-33-sigma"
        ;;
    3.11-ok)
        cpukeys="CPU_KEYS_SMP86xx_2011-09-22"
        kernelversion="2.6.22.19"
        kernelrelease="${kernelversion}-35-sigma"
        ;;
    3.11-nk)
        cpukeys="CPU_KEYS_SMP86xx_2011-09-22"
        kernelversion="2.6.32"
        kernelrelease="${kernelversion}.15-21-sigma"
        ;;
    4.0)
        cpukeys="CPU_KEYS_SMP86xx_2011-09-22"
        kernelversion="2.6.32"
        kernelrelease="${kernelversion}.15-21-sigma"
        ;;
    4.0-ok)
        cpukeys="CPU_KEYS_SMP86xx_2011-09-22"
        kernelversion="2.6.29"
        kernelrelease="${kernelversion}.6-33-sigma"
        ;;
esac
export cpukeys kernelversion kernelrelease

export ja_xsdk_dir="$ja_builddir/pkg/xsdk/$cpukeys"

# XSDK
export XSDK_ROOT="$ja_xsdk_dir/signed_items"
export XSDK_DEFAULT_KEY_DOMAIN=8644_ES1_prod
export XSDK_DEFAULT_ZBOOT_CERTID=0000
export XSDK_DEFAULT_CPU_CERTID=0001

if [ -d "$ja_xsdk_dir/xbin" ]; then
    PATH="$ja_xsdk_dir/xbin:$PATH"
fi

export kerneldir="$ja_srcdir/sigma-kernel"
export kernelmodulesdir="$sdk_rootfs_root/lib/modules/$kernelrelease"
export kernelextramodulesdir="$kernelmodulesdir/extra"

export LINUX_KERNEL="$kerneldir/linux"
export UCLINUX_KERNEL="$LINUX_KERNEL"
