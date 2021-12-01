#!/bin/sh

pkg_configure() {
    if [ -z "$pkg_source_dir" ]; then
        message "pkg_source_dir is not set, skipping configure"
        return 0
    fi

    local IFS="$jagen_IFS" S="$jagen_FS" A= MA="$(cat "${jagen_build_args_file:?}" 2>&-)"
    local option

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

            for option in $pkg_build_options; do
                A="$A$S$(eval echo $option)"
            done

            pkg_run "${pkg_build_cmake_executable:?}" \
                    -G"${pkg_build_cmake_generator:?}" \
                    --no-warn-unused-cli \
                    $A "$@" $MA "$pkg_source_dir"
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
