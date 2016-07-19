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

toolchain_generate_wrapper() {
    local wrapper="${1:?}" filepath="${2:?}"
    cat >"$wrapper" <<EOF || return
exec \$jagen_ccache "$filepath" "\$@"
EOF
    chmod +x "$wrapper"
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
            toolchain_generate_wrapper "$dest_path" "$src_path" || return
            chmod +x "$dest_path" || return
        else
            warning "$src_path is not found"
        fi
    done
}

# The logic here is taken from Buildroot external toolchain helpers.
toolchain_copy_sysroot() {
    local dest_dir="${1:-${jagen_target_dir:?}}"
    local sysroot_dir="$(toolchain_get_sysroot)"
    local arch_sysroot_dir="$(toolchain_get_arch_sysroot)"
    local arch_subdir="$(toolchain_sysroot_get_arch_subdir "$sysroot_dir" "$arch_sysroot_dir")"
    local arch_lib_dir="$(toolchain_get_arch_lib_dir)"
    local support_lib_dir="$(toolchain_get_support_lib_dir)"

    for dir in etc "$arch_lib_dir" sbin usr usr/"$arch_lib_dir"; do
        if [ -d "$arch_sysroot_dir/$dir" ]; then
            pkg_run rsync -a \
                --exclude 'usr/lib/locale' \
                --include '/libexec*/' \
                --exclude '/lib*/' \
                "$arch_sysroot_dir/$dir/" "$dest_dir/$dir/"
        fi
    done

    if [ "$sysroot_dir" != "$arch_sysroot_dir" ]; then
        if ! [ -d "$arch_sysroot_dir/usr/include" ]; then
            pkg_run rsync -a "$sysroot_dir/usr/include" "$dest_dir/usr"
        fi

        ( pkg_run cd "$dest_dir"
          pkg_run ln -snf "." "$arch_subdir" )
    fi

    if [ "$support_lib_dir" ]; then
        pkg_run rsync -a "$support_lib_dir/" "$dest_dir/lib"
    fi
}

toolchain_install_runtime() {
    local dest_dir="${1:-${jagen_target_dir:?}}/lib"
    local sysroot_dir="$(toolchain_get_arch_sysroot)"
    local lib_dir="$(toolchain_get_support_lib_dir)"
    local includes_file="$(find_file toolchain_lib_includes.txt)"
    local excludes_file="$(find_file toolchain_lib_excludes.txt)"

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
