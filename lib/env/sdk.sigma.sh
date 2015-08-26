#!/bin/sh

target_system="mipsel-linux-gnu"
target_prefix="/firmware"

target_arch="mips"
target_cpu="24kf"
target_board="${target_board:-ast100}"

toolchain_bin_dir="${target_dir}/bin"

sdk_ezboot_dir="$pkg_src_dir/sigma-ezboot"
sdk_kernel_dir="$pkg_src_dir/sigma-kernel"
sdk_mrua_dir="$pkg_src_dir/sigma-mrua"

sdk_rootfs_dir="$pkg_src_dir/sigma-rootfs"
sdk_rootfs_root="$sdk_rootfs_dir/build_mipsel/root"
sdk_rootfs_prefix="$sdk_rootfs_dir/cross_rootfs"

export SMP86XX_ROOTFS_PATH="$sdk_rootfs_dir"
export INSTALL_MOD_PATH="$sdk_rootfs_root"

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

kernel_release="2.6.32.15-21-sigma"
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

make_toolchain() {
    : ${jagen_toolchain_dir:?}

    local common_tools="addr2line ar c++filt elfedit gcov gdb gdbtui gprof nm \
objcopy objdump ranlib readelf size sprite strings strip"
    local inc_opt="-isystem \"$sdk_rootfs_prefix/include\""
    local lib_opt="-L\"$sdk_rootfs_prefix/lib\""

    local gcc_path="$jagen_toolchain_dir/bin/mips-linux-gnu-gcc"
    local gcc_dir=$(dirname "${gcc_path}")
    local ccache

    rm -fr "$toolchain_bin_dir"
    mkdir -p "$toolchain_bin_dir"

    make_tool ld -EL
    make_tool as -EL "$inc_opt"

    make_tool cpp -EL "$inc_opt $lib_opt"

    in_flags ccache && ccache='$jagen_ccache'

    for name in c++ g++ gcc; do
        make_tool $name -EL "$inc_opt $lib_opt"
    done

    ccache=""

    for name in $common_tools; do
        make_tool $name
    done

    chmod 755 "$toolchain_bin_dir"/* || return
}

make_tool() {
    local name="$1" pre_opt="$2" post_opt="$3"
    cat >"${toolchain_bin_dir}/${target_system}-${name}" <<EOF
#!/bin/sh
exec $ccache $gcc_dir/mips-linux-gnu-${name} $pre_opt "\$@" $post_opt
EOF
}
