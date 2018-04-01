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
    local include_opt=
    local lib_opt=

    make_tool ld -EL
    make_tool as -EL "$include_opt"

    make_tool cpp -EL "$include_opt $lib_opt"

    for name in c++ g++ gcc; do
        make_tool $name -EL "$include_opt $lib_opt"
    done

    for name in $common_tools; do
        make_tool $name
    done
}
