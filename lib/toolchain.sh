#!/bin/sh

toolchain_programs='
addr2line
ar
as
c++
c++filt
cpp
g++
gcc
gccbug
gcov
gdb
gprof
ld
nm
objcopy
objdump
ranlib
readelf
size
strings
strip
'

toolchain_cc() {
    printf "${1:-${CC:-${pkg_build_system:+${pkg_build_system}-}gcc}}"
}

toolchain_get_sysroot() {
    real_path $("$(toolchain_cc "$1")" --print-sysroot)
}

toolchain_get_arch_sysroot() {
    real_path $("$(toolchain_cc "$1")" $pkg_build_cflags --print-sysroot)
}

toolchain_find_path() {
    local filename="$("$(toolchain_cc "$2")" $pkg_build_cflags --print-file-name="${1:?}")"
    if [ "$filename" ]; then
        real_path $(dirname "$filename")
    fi
}

toolchain_get_lib_dir() {
    toolchain_find_path libstdc++.a "$@"
}

toolchain_generate_wrappers() {
    local dest_dir="${1:?}"
    local src_dir="${2:?}"
    local prefix="$3"
    local name src_path dest_path programs

    [ -d "$dest_dir" ] || \
        die "toolchain_generate_wrappers: the dest dir '$dest_dir' does not exist"
    [ -d "$src_dir" ] || \
        die "toolchain_generate_wrappers: the src dir '$src_dir' does not exist"

    if [ "$prefix" ]; then
        programs=$(cd "$src_dir" && find . -type f -maxdepth 1 -name "${prefix}*" | \
            sed -r "s|./${prefix}(.+)|\1|")
    else
        programs="$toolchain_programs"
    fi

    for name in $programs; do
        src_path="${src_dir}/${prefix}${name}"
        dest_path="${dest_dir}/${prefix}${name}"
        if [ -x "$src_path" ]; then
            cat >"$dest_path" <<EOF || return
exec \$jagen_ccache "$src_path" "\$@"
EOF
            chmod +x "$dest_path" || return
        fi
    done
}

toolchain_create_alias() {
    local source="${1:?}"
    local target="${2:?}"
    local from="$(basename "$target")"
    local to="$(basename "$source")"
    local name

    [ "$source" = "$target" ] && return

    cd $(dirname "$target")

    for name in ${toolchain_programs:?}; do
        if [ -x "$from$name" ]; then
            ln -snfr $from$name $to$name
        fi
    done

    cd "$OLDPWD"
}

toolchain_install_runtime() {
    local dest_dir="${1:-${jagen_target_dir:?}}/lib"
    local sysroot_dir="$(toolchain_get_arch_sysroot)"
    local lib_dir="$(toolchain_get_lib_dir)"
    local filter_file="$(find_in_path toolchain_libs.txt)"

    : ${sysroot_dir:?}
    : ${lib_dir:?}
    : ${filter_file:?}

    message "install toolchain runtime: $sysroot_dir, $lib_dir to $dest_dir"

    pkg_run rsync -va --chmod=F0755 \
        --filter="merge $filter_file" \
        "$sysroot_dir/lib/" "$dest_dir"

    pkg_run rsync -va --chmod=F0755 \
        --filter="merge $filter_file" \
        "$lib_dir/" "$dest_dir"
}

toolchain_install_ldconfig() {
    :
}

toolchain_install_android_standalone() {
    local make_script="${toolchain_source_dir:?}/build/tools/make_standalone_toolchain.py"
    "$make_script" --force \
                   --arch "${pkg_build_arch:?}" \
                   --install-dir "${pkg_build_dir:?}"
}
