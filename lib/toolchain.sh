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
        case $filename in
            ${prefix}${name}-*|${prefix}${name})
                return 0 ;;
        esac
    done
    return 1
}

toolchain_wrap() {
    local dest_dir="${1:?}" path="${2:?}" varname="$3"
    local cmd="$path" filename="${path##*/}"
    local wrapper="${dest_dir}/${filename}"
    if [ "$varname" ]; then
        message "wrap ${wrapper#$jagen_root_dir/} -> ${path#$jagen_root_dir/}"
        case $varname in
            cflags|cxxflags)
                # compiler is often called as a linker, so it needs ldflags too
                cmd="$cmd \$pkg_toolchain_$varname \$pkg_toolchain_ldflags" ;;
            *)
                cmd="$cmd \$pkg_toolchain_$varname" ;;
        esac
        # if the wrapper is a symlink echo will overwrite the original
        # executable in the toolchain unless we remove the link first
        rm -f "$wrapper" || return
        echo "exec \$jagen_ccache $cmd \"\$@\""  >"$wrapper" || return
        chmod +x "$wrapper" || return
    else
        message "link ${wrapper#$jagen_root_dir/} -> ${path}"
        ln -sf "$path" "$wrapper" || return
    fi
}

toolchain_generate_wrappers() {
    local src_dir="${1:?}" dest_dir="${jagen_bin_dir}/${pkg_name}"
    local PATH="$(IFS=: list_remove "$dest_dir" "$PATH")"
    local IFS="$jagen_IFS" path pathnames
    local c_names="$toolchain_names_c"
    local cpp_names="$toolchain_names_cpp"
    local cxx_names="$toolchain_names_cxx"
    local linker_names="$toolchain_names_ld"

    [ -d "$src_dir" ] || \
        die "toolchain_generate_wrappers: the src dir '$src_dir' does not exist"

    if [ "$pkg_export_system" ]; then
        # this is a very common layout
        pathnames=$(find "$src_dir/bin" -maxdepth 1 -type f -executable \
                         -name "${pkg_export_system}-*" 2>/dev/null)
        # something not common, try to a deep search
        if [ -z "$pathnames" ]; then
            pathnames=$(find "$src_dir" -maxdepth 10 -type f -executable \
                             -path '*/bin/*' -name "${pkg_export_system}-*" 2>/dev/null)
        fi
        [ "$pathnames" ] || die "Failed to find any ${pkg_export_system}-* toolchain executables in $src_dir"
    else
        for path in $c_names $cpp_names $cxx_names $linker_names; do
            path=$(command -v "$path")
            if [ "$path" ]; then
                pathnames="${pathnames}${jagen_S}${path}"
            fi
        done
    fi

    pkg_run mkdir -p "$dest_dir"

    for path in $pathnames; do
        if toolchain_match "$path" $c_names; then
            toolchain_wrap "$dest_dir" "$path" cflags
        elif toolchain_match "$path" $cpp_names; then
            toolchain_wrap "$dest_dir" "$path" cflags
        elif toolchain_match "$path" $cxx_names; then
            toolchain_wrap "$dest_dir" "$path" cxxflags
        elif toolchain_match "$path" $linker_names; then
            toolchain_wrap "$dest_dir" "$path" ldflags
        else
            toolchain_wrap "$dest_dir" "$path"
        fi
    done
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
