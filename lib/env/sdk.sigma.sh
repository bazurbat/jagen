#!/bin/sh

export rootfsdir="$ja_srcdir/sigma-rootfs"

toolchain_gcc=$(which mips-linux-gnu-gcc >/dev/null 2>&1)
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

if [ "$buildtype" = "Debug" ]; then
    COMPILKIND="debug glibc codesourcery hardfloat"
else
    COMPILKIND="release glibc codesourcery hardfloat"
fi
export COMPILKIND

# XSDK
export XSDK_ROOT="$sdkdir/$cpukeys/signed_items"
export XSDK_DEFAULT_KEY_DOMAIN=8644_ES1_prod
export XSDK_DEFAULT_ZBOOT_CERTID=0000
export XSDK_DEFAULT_CPU_CERTID=0001

export LINUX_KERNEL="$kerneldir/linux"
export UCLINUX_KERNEL="$LINUX_KERNEL"
