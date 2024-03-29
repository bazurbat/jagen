#!/bin/sh

pkg_compile() {
    local IFS="$jagen_IFS" S="$jagen_FS" A= MA="$(cat "$jagen_build_args_file" 2>&-)"

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
            pkg_run "${pkg_build_cmake_executable:?}" --build . -- $(pkg__get_cmake_args) "$@" $MA
            ;;
        make|kbuild)
            if [ -f "${pkg_source_dir:?}/GNUmakefile" ]; then
                makefile="$pkg_source_dir/GNUmakefile"
            elif [ -f "${pkg_source_dir:?}/makefile" ]; then
                makefile="$pkg_source_dir/makefile"
            elif [ -f "${pkg_source_dir:?}/Makefile" ]; then
                makefile="$pkg_source_dir/Makefile"
            fi
            if [ "$makefile" ]; then
                A="$A${S}-f$makefile"
            fi
            pkg_run make $A $pkg_build_options "$@" $MA
            ;;
        linux-kernel)
            pkg_run cd "${pkg_source_dir:?}"
            if [ "$pkg_build_image" ]; then
                pkg_run make $pkg_build_image $MA
            fi
            pkg_run make modules $MA
            ;;
        linux-module)
            pkg_run make $pkg_build_options "$@" $MA
            ;;
        executable)
            local exe="$jagen_dist_dir/$pkg_source_filename"
            if ! [ -f "$exe" ]; then
                die "require to run $exe for build but the file was not found"
            fi
            pkg_run chmod +x "$exe"
            pkg_run "$exe" $pkg_build_options "$@" $MA
            ;;
        rust)
            if [ "$CARGO_HOME" ]; then
                PATH="$CARGO_HOME/bin:$PATH"
            fi
            export CARGO_TARGET_DIR="$pkg_build_dir"
            cd "${pkg_source_dir:?}"
            pkg_is_release && A="--release"
            if [ "$pkg_build_rust_toolchain" = 'system' ]; then
                debug2 "using cargo from $pkg_build_rust_toolchain: $(which cargo)"
                pkg_run cargo build ${pkg_build_system:+--target=$pkg_build_system} \
                    $A "$@" $MA
            else
                debug2 "using rustup from $pkg_build_rust_toolchain: $(which rustup)"
                pkg_run rustup run "${pkg_build_rust_toolchain:?}" \
                    cargo build ${pkg_build_system:+--target=$pkg_build_system} \
                    $A "$@" $MA
            fi
            ;;
        android-gradle)
            if ! [ -f "${pkg_source_dir:?}/gradlew" ]; then
                die "failed to find Gradle wrapper (gradlew) script in the project's source directory: $pkg_source_dir"
            fi
            if pkg_is_release; then
                A="assembleRelease"
            else
                A="assembleDebug"
            fi
            pkg_run bash "${pkg_source_dir:?}/gradlew" $A "$@" $MA
            ;;
        android-standalone-toolchain)
            require toolchain
            toolchain_install_android_standalone
            ;;
    esac
}
