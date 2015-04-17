#!/bin/sh

common_tools="addr2line ar c++filt elfedit gcov gdb gdbtui gprof nm objcopy
objdump ranlib readelf size sprite strings strip"

inc_opt="-isystem \"$sdk_rootfs_prefix/include\""
lib_opt="-L\"$sdk_rootfs_prefix/lib\""

generate_toolchain_wrappers() {
    mkdir -p "$target_bin_dir"

    generate_tool ld -EL
    generate_tool as -EL "$inc_opt"

    generate_tool cpp -EL "$inc_opt $lib_opt"

    in_flags ccache && ccache="ccache"

    for name in c++ g++ gcc; do
        generate_tool $name -EL "$inc_opt $lib_opt"
    done

    ccache=""

    for name in $common_tools; do
        generate_tool $name
    done

    chmod 755 "$target_bin_dir"/*
}

generate_tool() {
    local name="$1" pre_opt="$2" post_opt="$3"
    cat <<EOF >"${target_bin_dir}/${target_system}-${name}"
#!/bin/sh
exec $ccache mips-linux-gnu-${name} $pre_opt "\$@" $post_opt
EOF
}
