#!/bin/sh

pkg_install() {
    [ "$pkg_source_dir" ] || return 0

    local IFS="$jagen_IFS" MA="$(cat "${jagen_build_args_file:?}" 2>&-)"
    local pkg_install_type="${pkg_install_type:-$pkg_build_type}"

    case $pkg_install_type in
        gnu|make|kbuild)
            pkg_run make \
                ${pkg_install_root:+DESTDIR="$pkg_install_root"} \
                $pkg_install_args "$@" $MA install

            for name in $pkg_install_libs; do
                pkg_fix_pc "$name"
                # pkg_fix_la "$pkg_install_root$pkg_install_prefix/lib/lib${name}.la" "$pkg_install_root"

                if [ -z "$pkg_install_config_script" ]; then
                    pkg_install_config_script="/bin/${pkg_name}-config"
                fi
                pkg_fix_config_script "${pkg_install_dir:?}${pkg_install_config_script}"
            done
            ;;
        cmake)
            if [ "$pkg_install_root" ]; then
                export DESTDIR="$pkg_install_root"
            fi
            pkg_run "${pkg_build_cmake_executable:?}" --build . --target install -- \
                $(pkg__get_cmake_args) $pkg_install_args "$@" $MA
            unset DESTDIR
            ;;
        linux-kernel)
            use_env kbuild
            pkg_run cd "$pkg_source_dir"
            pkg_run install -vm644 \
                "$pkg_build_dir/arch/$pkg_build_arch/boot/${pkg_build_image:?}" \
                "$jagen_build_dir"
            pkg_run make \
                INSTALL_MOD_PATH="${INSTALL_MOD_PATH:-${pkg_install_dir:?}}" \
                $pkg_install_args "$@" $MA modules_install
            ;;
        linux-module)
            pkg_install_modules
            ;;
        toolchain)
            require toolchain
            toolchain_generate_wrappers    \
                "${pkg_source_dir:?}/bin"  \
                "${pkg_toolchain_prefix}"
            ;;
        rust-toolchain)
            if [ "$pkg_build_name" ]; then
                if ! rustup toolchain list | grep -q "^$pkg_build_name"; then
                    rustup install "$pkg_build_name" ||
                        die "failed to install Rust toolchain: $pkg_build_name"
                fi
            fi
            if [ "$pkg_build_system" ]; then
                if ! rustup target list | grep -q "^${pkg_build_system}.*(installed)"; then
                    rustup target add "$pkg_build_system" ||
                        die "failed to add Rust target: $pkg_build_system"
                fi
            fi
            ;;
        android-standalone-toolchain)
            require toolchain
            toolchain_generate_wrappers    \
                "${pkg_build_dir:?}/bin"   \
                "${pkg_toolchain_prefix}"
            ;;
    esac

    if [ "$pkg_install_ldconfig" ]; then
        pkg_run_ldconfig
    fi
}

pkg__modules_install() {
    pkg_run make -C "${KERNEL_SRC:?}" M="$PWD/$1" \
        INSTALL_MOD_PATH="${INSTALL_MOD_PATH:-${pkg_install_dir:?}}" \
        modules_install
}

pkg_install_modules() {
    local dir

    if [ "$pkg_install_module_dirs" ]; then
        for dir in $pkg_install_module_dirs; do
            pkg__modules_install "$dir"
        done
    else
        # NOTE: Passing '.' here causes 'modules_install' to append
        # full path to the module sources after install-dir.
        pkg__modules_install ''
    fi
}
