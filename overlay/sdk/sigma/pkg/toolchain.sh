#!/bin/sh

make_tool() {
    local name="$1" pre_opt="$2" post_opt="$3"
    cat >"${toolchain_dir}/bin/${target_system}-${name}" <<EOF
#!/bin/sh
exec $ccache $gcc_dir/mips-linux-gnu-${name} $pre_opt "\$@" $post_opt
EOF
}

jagen_pkg_install() {
    : ${jagen_toolchain_dir:?}

    local common_tools="addr2line ar c++filt elfedit gcov gdb gdbtui gprof nm \
objcopy objdump ranlib readelf size sprite strings strip"
    local inc_opt="-isystem \"$sdk_rootfs_prefix/include\""
    local lib_opt="-L\"$sdk_rootfs_prefix/lib\""

    local gcc_path="$jagen_toolchain_dir/bin/mips-linux-gnu-gcc"
    local gcc_dir=$(dirname "${gcc_path}")
    local ccache

    rm -fr "$toolchain_dir/bin"
    mkdir -p "$toolchain_dir/bin"

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

    chmod 755 "$toolchain_dir"/bin/* || return
}
