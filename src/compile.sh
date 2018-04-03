#!/bin/sh

pkg_compile() {
    local IFS="$jagen_IFS" S="$jagen_FS" A= MA="$(cat "${jagen_build_args_file:?}" 2>&-)"

    [ "$pkg_source_dir" ] || return 0

    local is_offline= verbose_opt= jobs=
    local makefile=

    if in_flags offline; then
        is_offline=1
    fi

    case $pkg_build_type in
        gnu)
            pkg_run make "$@" $MA
            ;;
        cmake)
            pkg_run cmake --build . -- $(pkg__get_cmake_args) "$@" $MA
            ;;
        make|kbuild)
            if [ -f "$pkg_source_dir/GNUmakefile" ]; then
                makefile="$pkg_source_dir/GNUmakefile"
            elif [ -f "$pkg_source_dir/makefile" ]; then
                makefile="$pkg_source_dir/makefile"
            elif [ -f "$pkg_source_dir/Makefile" ]; then
                makefile="$pkg_source_dir/Makefile"
            fi
            if [ "$makefile" ]; then
                A="$A${S}-f$makefile"
            fi
            pkg_run make $A $pkg_build_options "$@" $MA
            ;;
        linux-kernel)
            use_env kbuild
            pkg_run cd "$pkg_source_dir"
            pkg_run make "${pkg_build_image:?}" modules $MA
            ;;
        linux-module)
            pkg_run make $pkg_build_options "$@" $MA
            ;;
        executable)
            local exe="$jagen_dist_dir/$pkg_source_filename"
            if ! [ -x "$exe" ]; then
                die "require to run $exe for build but the file was not found or not an executable"
            fi
            pkg_run "$exe" $pkg_build_options "$@" $MA
            ;;
        rust)
            export CARGO_TARGET_DIR="$pkg_build_dir"
            cd "$pkg_source_dir"
            pkg_is_release && A="--release"
            pkg_run cargo build ${pkg_build_system:+--target=$pkg_build_system} \
                $A "$@" $MA
            ;;
        android-standalone-toolchain)
            require toolchain
            toolchain_install_android_standalone
            ;;
    esac
}
