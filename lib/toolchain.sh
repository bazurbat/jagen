#!/bin/sh

toolchain_get_sysroot() {
    real_path $("${1:-${CC:?}}" --print-sysroot)
}

toolchain_get_arch_sysroot() {
    real_path $("${1:-${CC:?}}" $ARCH_CFLAGS --print-sysroot)
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
    local libstdc="$("${1:-${CC:?}}" $ARCH_CFLAGS --print-file-name=libstdc++.a)"
    if [ "$libstdc" ]; then
        real_path $(dirname "$libstdc")
    fi
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
    local includes_file="$(jagen_find_path toolchain_lib_includes.txt)"
    local excludes_file="$(jagen_find_path toolchain_lib_excludes.txt)"

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
