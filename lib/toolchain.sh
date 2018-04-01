#!/bin/sh

toolchain_programs='
addr2line
ar
as
c++
c++filt
clang
clang++
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
    local src_dir="${1:?}" prefix="$2" dest_dir="${jagen_bin_dir:?}"
    local IFS="$jagen_IFS" S="$jagen_S"
    local PATH="$(list_remove : "$dest_dir" $PATH)"
    local item paths dest

    [ -d "$dest_dir" ] || \
        die "toolchain_generate_wrappers: the dest dir '$dest_dir' does not exist"
    [ -d "$src_dir" ] || \
        die "toolchain_generate_wrappers: the src dir '$src_dir' does not exist"

    if [ "$prefix" ]; then
        paths=$(find "$src_dir" -type f -maxdepth 1 -name "${prefix}*")
        [ "$paths" ] || die "Failed to find any files matching ${prefix}* in $src_dir, \
no toolchain wrappers will be generated. Please verify that the specified toolchain build \
system and source directory are correct."
    else
        for item in $toolchain_programs; do
            item=$(command -v "$item")
            if [ "$item" ]; then
                paths="${paths}${S}${item}"
            fi
        done
    fi

    for item in $paths; do
        dest="${dest_dir}/${item##*/}"
        cat >"$dest" <<EOF || return
exec \$jagen_ccache "$item" "\$@"
EOF
        chmod +x "$dest" || return
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
