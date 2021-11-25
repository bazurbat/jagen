#!/bin/sh
#shellcheck disable=2154,2155

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

toolchain_match() {
    local path="${1:?}"
    local name filename="${path##*/}" prefix="${pkg_export_system:+${pkg_export_system}-}"
    local IFS="$jagen_FS"
    shift
    for name; do
        echo F=$filename N=\"$name\" P=\"$prefix\"
        case $filename in
            ${prefix}${name}-*|${prefix}${name})
                echo match
                return 0 ;;
        esac
    done
    return 1
}

toolchain_find() {
    local src_dir="${1:?}" name="${2:?}" path=
    # this is a very common layout
    path=$(find "$src_dir/bin" -maxdepth 1 -type f -executable \
                -name "$name" 2>/dev/null)
    [ "$path" ] && { echo "$path"; return; }
    # something not common, try to a deep search
    path=$(find "$src_dir" -maxdepth 10 -type f -executable \
                -path '*/bin/*' -name "$name" 2>/dev/null)
    [ "$path" ] && { echo "$path"; return; }
    # fallback to PATH
    path=$(command -v "$path")
    echo "$path"
}

toolchain_wrap() {
    local dest_dir="${1:?}" path="${2:?}" varname="${3:?}"
    local filename="${path##*/}" cmd=
    local wrapper="${dest_dir}/${filename:?}"
    message "wrap ${wrapper#$jagen_root_dir/} -> ${path#$jagen_root_dir/}"
    case $varname in
        cflags|cxxflags)
            # compiler is often called as a linker, so it needs ldflags too
            cmd="$path \$pkg_toolchain_$varname \$pkg_toolchain_ldflags" ;;
        *)
            cmd="$path \$pkg_toolchain_$varname" ;;
    esac
    # if the wrapper is a symlink echo will overwrite the original
    # executable in the toolchain unless we remove the link first
    rm -f "$wrapper" || return
    echo "exec $cmd \"\$@\""  >"$wrapper" || return
    chmod +x "$wrapper" || return
}

toolchain_generate_wrappers() {
    local src_dir="${1:?}" dest_dir="${jagen_bin_dir:?}/${pkg_name}"

    [ -d "$src_dir" ] || \
        die "toolchain_generate_wrappers: the src dir '$src_dir' does not exist"

    pkg_run mkdir -p "$dest_dir"

    toolchain_wrap "$dest_dir" $(toolchain_find "$src_dir" "$pkg_export_cc")  cflags
    toolchain_wrap "$dest_dir" $(toolchain_find "$src_dir" "$pkg_export_cxx") cxxflags
    toolchain_wrap "$dest_dir" $(toolchain_find "$src_dir" "$pkg_export_cpp") cflags
    toolchain_wrap "$dest_dir" $(toolchain_find "$src_dir" "$pkg_export_ld")  ldflags
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
    unset IFS
    local make_script="${toolchain_source_dir:?}/build/tools/make_standalone_toolchain.py"
    pkg_run "$make_script" --force \
        --arch "${pkg_build_arch:?}" \
        ${pkg_build_android_api:+--api "$pkg_build_android_api"} \
        --install-dir "${pkg_build_dir:?}"
}
