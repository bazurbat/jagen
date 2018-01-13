#!/bin/sh

require toolchain

make_tool() {
    local name="${1:?}" pre_opt="$2" post_opt="$3"
    local path="${jagen_bin_dir:?}/${pkg_build_system:?}-${name}"
    cat >"$path" <<EOF || return
exec \$jagen_ccache "${pkg_source_dir:?}/bin/mips-linux-gnu-${name}" $pre_opt "\$@" $post_opt
EOF
    chmod +x "$path"
}

jagen_pkg_install_target() {
    local common_tools="addr2line ar c++filt elfedit gcov gdb gdbtui gprof nm \
objcopy objdump ranlib readelf size sprite strings strip"
    # These might be needed for compatibility with bundled rootfs, we are
    # trying to migrate from this - keep disabled for now
    local inc_opt= # "-isystem \"\$jagen_sdk_rootfs_prefix/include\""
    local lib_opt= # "-L\"\$jagen_sdk_rootfs_prefix/lib\""

    make_tool ld -EL
    make_tool as -EL "$inc_opt"

    make_tool cpp -EL "$inc_opt $lib_opt"

    for name in c++ g++ gcc; do
        make_tool $name -EL "$inc_opt $lib_opt"
    done

    for name in $common_tools; do
        make_tool $name
    done
}
