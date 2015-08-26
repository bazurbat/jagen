#!/bin/sh

toolchain_bin_dir="${target_dir}/bin"

export AR="${toolchain_bin_dir}/${target_system}-ar"
export CC="${toolchain_bin_dir}/${target_system}-gcc"
export CXX="${toolchain_bin_dir}/${target_system}-g++"
export STRIP="${toolchain_bin_dir}/${target_system}-strip"

CFLAGS="-O2 -fomit-frame-pointer -fno-strict-aliasing"
CFLAGS="$CFLAGS -Wa,-mips32r2 -march=24kf -mtune=24kf -pipe"
export CFLAGS
export CXXFLAGS="$CFLAGS"
export ASMFLAGS="$CFLAGS"
export LDFLAGS=""

export PKG_CONFIG_SYSROOT_DIR="$target_dir"
export PKG_CONFIG_LIBDIR="$target_dir$target_prefix/lib/pkgconfig"
export PKG_CONFIG_PATH="$sdk_rootfs_prefix/lib/pkgconfig"

make_toolchain() {
    local common_tools="addr2line ar c++filt elfedit gcov gdb gdbtui gprof nm \
objcopy objdump ranlib readelf size sprite strings strip"
    local inc_opt="-isystem \"$sdk_rootfs_prefix/include\""
    local lib_opt="-L\"$sdk_rootfs_prefix/lib\""

    local gcc_path="$jagen_toolchain_dir/bin/mips-linux-gnu-gcc"
    local gcc_dir=$(dirname "${gcc_path}")
    local ccache

    if ! [ "$jagen_toolchain_dir" ]; then
        error "jagen_toolchain_dir is not set"
        return 1
    fi
    if ! [ -x "$gcc_path" ]; then
        error "${gcc_path} is not found"
        return 1
    fi

    mkdir -p "$toolchain_bin_dir" || return

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
