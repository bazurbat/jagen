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

toolchain_get_sysroot() {
    real_path $("${1:-${CC:?}}" --print-sysroot)
}

toolchain_get_arch_sysroot() {
    real_path $("${1:-${CC:?}}" $jagen_target_cflags --print-sysroot)
}

toolchain_sysroot_get_arch_subdir() {
    local sysroot="${1:-$(toolchain_get_sysroot)}"
    local arch_sysroot="${2:-$(toolchain_get_arch_sysroot)}"
    printf "$arch_sysroot" | sed -re "s|$sysroot/||"
}

toolchain_get_arch_lib_dir() {
    # TODO: check different toolchains
    printf "lib"
}

toolchain_find_path() {
    local pathname="$("${2:-${CC:?}}" $jagen_target_cflags --print-file-name="${1:?}")"
    if [ "$pathname" ]; then
        real_path $(dirname "$pathname")
    fi
}

toolchain_get_support_lib_dir() {
    local libstdc="$("${1:-${CC:?}}" $jagen_target_cflags --print-file-name=libstdc++.a)"
    if [ "$libstdc" ]; then
        real_path $(dirname "$libstdc")
    fi
}

toolchain_unpack() {
    : ${jagen_toolchains_dir:?}
    local name="${1:?}"
    local source_dir="${2:?}"
    local target_dir="$jagen_toolchains_dir/$(basename "$source_dir")"
    local work_dir="$pkg_work_dir"
    local pkg_work_dir="$jagen_toolchains_dir"

    if ! [ -d "$target_dir" ]; then
        pkg_unpack
    fi

    pkg_run mkdir -p "$work_dir"
    pkg_link "$target_dir" "$source_dir"
}

toolchain_generate_wrappers() {
    local dest_dir="${1:?}"
    local src_dir="${2:?}"
    local prefix="$3"
    local name src_path dest_path

    for name in ${toolchain_programs:?}; do
        dest_path="${dest_dir}/${prefix:+$prefix-}${name}"
        src_path="${src_dir}/${prefix:+$prefix-}${name}"
        if [ -x "$src_path" ]; then
            cat >"$dest_path" <<EOF || return
exec \$jagen_ccache "$src_path" "\$@"
EOF
            chmod +x "$dest_path" || return
        else
            warning "$src_path is not found"
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
    local CC="${CC:-${jagen_target_system:+$jagen_target_system-}gcc}"
    local dest_dir="${1:-${jagen_target_dir:?}}/lib"
    local sysroot_dir="$(toolchain_get_arch_sysroot)"
    local lib_dir="$(toolchain_get_support_lib_dir)"
    local includes_file="$(find_in_path toolchain_lib_includes.txt)"
    local excludes_file="$(find_in_path toolchain_lib_excludes.txt)"

    : ${sysroot_dir:?}
    : ${lib_dir:?}
    : ${includes_file:?}
    : ${excludes_file:?}

    message "install toolchain runtime: $sysroot_dir, $lib_dir to $dest_dir"

    pkg_run rsync -a \
        --chmod=F0755 \
        --exclude-from="$excludes_file" \
        --include-from="$includes_file" \
        --exclude="*" \
        "$sysroot_dir/lib/" "$dest_dir"

    if [ "$sysroot_dir" != "$lib_dir" ]; then
        pkg_run rsync -a \
            --chmod=F0755 \
            --exclude-from="$excludes_file" \
            --include-from="$includes_file" \
            --exclude="*" \
            "$lib_dir/" "$dest_dir"
    fi
}

toolchain_install_ldconfig() {
    :
}
