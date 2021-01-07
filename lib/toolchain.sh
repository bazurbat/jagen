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
    local path="${1:?}" list_file="${2:?}"
    local name filename=${path##*/}
    while IFS= read -r name; do
        case $filename in
            ${pkg_toolchain_prefix}${name}-*|${pkg_toolchain_prefix}${name})
                return 0 ;;
        esac
    done <"$list_file"
    return 1
}

toolchain_wrap() {
    local dest_dir="${1:?}" path="${2:?}" varname="$3"
    local cmd="$path" filename="${path##*/}"
    local wrapper="${dest_dir}/${filename}"
    if [ "$varname" ]; then
        message "${wrapper#$jagen_root_dir/} -> ${path#$jagen_root_dir/}"
        case $varname in
            cflags|cxxflags)
                # compiler is often called as a linker, so it needs ldflags too
                cmd="$cmd \$jagen_pkg__$varname \$jagen_pkg__ldflags" ;;
            *)
                cmd="$cmd \$jagen_pkg__$varname" ;;
        esac
        echo "exec \$jagen_ccache $cmd \"\$@\""  >"$wrapper" || return
        chmod +x "$wrapper" || return
    else
        ln -sf "$path" "$wrapper" || return
    fi
}

toolchain_generate_wrappers() {
    local src_dir="${1:?}" dest_dir="${jagen_bin_dir}/${pkg_name}"
    local PATH="$(IFS=: list_remove "$dest_dir" "$PATH")"
    local IFS="$jagen_S" path pathnames
    local c_names=$(find_in_path toolchain/c_compiler_names)
    local cxx_names=$(find_in_path toolchain/cxx_compiler_names)
    local linker_names=$(find_in_path toolchain/linker_names)

    [ -d "$src_dir" ] || \
        die "toolchain_generate_wrappers: the src dir '$src_dir' does not exist"

    if [ "$pkg_toolchain_prefix" ]; then
        pathnames=$(find "$src_dir/bin" "$src_dir" -maxdepth 1 \
                         -type f -executable -name "${pkg_toolchain_prefix}*")
        [ "$pathnames" ] || die "Failed to find any ${pkg_toolchain_prefix}* toolchain executables in $src_dir"
    else
        for path in $(cat "$c_names" "$cxx_names" "$linker_names"); do
            path=$(command -v "$path")
            if [ "$path" ]; then
                pathnames="${pathnames}${jagen_S}${path}"
            fi
        done
    fi

    pkg_run mkdir -p "$dest_dir"

    for path in $pathnames; do
        if toolchain_match "$path" "$c_names"; then
            toolchain_wrap "$dest_dir" "$path" cflags
        elif toolchain_match "$path" "$cxx_names"; then
            toolchain_wrap "$dest_dir" "$path" cxxflags
        elif toolchain_match "$path" "$linker_names"; then
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
