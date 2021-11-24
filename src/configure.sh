#!/bin/sh

pkg_configure() {
    if [ -z "$pkg_source_dir" ]; then
        message "pkg_source_dir is not set, skipping configure"
        return 0
    fi

    local IFS="$jagen_IFS" S="$jagen_FS" A= MA="$(cat "${jagen_build_args_file:?}" 2>&-)"
    local cmake_config=RELEASE

    case $pkg_build_type in
        gnu)
            pkg_run "${pkg_build_configure_file:-$pkg_source_dir/configure}" $A \
                ${pkg_build_system:+--host="$pkg_build_system"} \
                --prefix="$pkg_install_prefix" \
                --disable-dependency-tracking \
                ${pkg_install_root:+--with-sysroot="$pkg_install_root"} \
                $pkg_build_options "$@" $MA

            # Never add RPATH to generated binaries because libtool uses
            # various heuristics to determine when to add it, some Linux
            # distributions patch it to adjust a list of 'system' paths, but
            # generally things seems to work because everyone install to
            # /usr/local/lib or /usr/lib or lib64 whatever and these are
            # handled specially. Embedded systems often have different
            # conventions and naming schemes, libtool not always does the
            # 'right' thing and you might end up with a mixed bag of libraries
            # some having RPATH and some not.

            if [ -x "./libtool" ]; then
                pkg_run sed -i 's|\(hardcode_into_libs\)=yes|\1=no|g' \
                    "./libtool"
            fi

            ;;
        cmake)
            if ! [ -f "$pkg_source_dir/CMakeLists.txt" ]; then
                die "CMake build type specified but no CMakeLists.txt was found in $pkg_source_dir"
            fi

            # This can be imported from a toolchain, the placement here is
            # important to be able to override CFLAGS.
            if [ "$pkg_build_cmake_options" ]; then
                for option in $pkg_build_cmake_options; do
                    A="$A$S$option"
                done
            fi

            if $(jagen__versions ge "$(jagen__get_cmake_version)" 3.1); then
                A="$A$S-DCMAKE_EXPORT_NO_PACKAGE_REGISTRY=YES"
                A="$A$S-DCMAKE_FIND_PACKAGE_NO_PACKAGE_REGISTRY=YES"
                A="$A$S-DCMAKE_FIND_PACKAGE_NO_SYSTEM_PACKAGE_REGISTRY=YES"
            fi

            if $(jagen__versions ge "$(jagen__get_cmake_version)" 3.5); then
                A="$A$S-DCMAKE_EXPORT_COMPILE_COMMANDS=YES"
            fi

            pkg_run "${pkg_build_cmake_executable:-cmake}" -G"${pkg_build_generator:-Ninja}" \
                --no-warn-unused-cli \
                -DCMAKE_BUILD_TYPE="$(pkg_cmake_build_type)" \
                -DCMAKE_INSTALL_PREFIX="$pkg_install_prefix" \
                $A $pkg_build_options "$@" $MA "$pkg_source_dir"
            ;;
        linux-kernel)
            pkg_run cd "$pkg_source_dir"
            pkg_run make "${pkg_build_config:?build.config is not set}"
            pkg_run make prepare
            ;;
        *)
            ;;
    esac
}
