#!/bin/sh

. "$jagen_dir/lib/env.sh" ||
    { echo "Failed to load environment"; exit 1; }

: ${pkg_run_jobs:=$(nproc)}
: ${pkg_run_on_error:=exit}

pkg_run() {
    local cmd="$1"
    debug "$*"
    shift

    case $cmd in
        make)
            cmd="$cmd -j$pkg_run_jobs"
            [ "$jagen_build_verbose" = "yes" ] && cmd="$cmd V=1"
            ;;
        ninja)
            cmd="$cmd -j$pkg_run_jobs"
            [ "$jagen_build_verbose" = "yes" ] && cmd="$cmd -v"
            ;;
    esac

    $cmd "$@" || $pkg_run_on_error
}

pkg_patch() {
    local num="${1:?}" name="${2:?}"
    pkg_run patch -p$num -i "$jagen_patch_dir/${name}.patch"
}

pkg_strip_dir() {
    local root files
    root="$1"
    files=$(find "$root" -type f -not -name "*.ko" \
        "(" -path "*/lib*" -o -path "*/bin*" -o -path "*/sbin*" ")" | \
        xargs -r file | grep "ELF.*\(executable\|shared object\).*not stripped" | cut -d: -f1)

    for f in $files; do
        pkg_run "$STRIP" -v --strip-unneeded \
            -R .comment \
            -R .GCC.command.line \
            -R .note.gnu.gold-version \
            "$f"
    done
}

pkg_install_modules() {
    mkdir -p "$jagen_kernel_extra_modules_dir"
    touch "$jagen_kernel_modules_dir/modules.order"
    touch "$jagen_kernel_modules_dir/modules.builtin"
    for m in "$@"; do
        local f="$PWD/${m}.ko"
        cp "$f" "$jagen_kernel_extra_modules_dir"
    done &&
        (
    cd $jagen_kernel_dir/linux && \
        /sbin/depmod -ae -F System.map -b $INSTALL_MOD_PATH $jagen_kernel_release
    )
}

pkg_run_depmod() {
    pkg_run /sbin/depmod -ae \
        -F "$LINUX_KERNEL/System.map" \
        -b "$INSTALL_MOD_PATH" \
        "$jagen_kernel_release"
}

pkg_fix_la() {
    local filename="${1:?}" prefix="$2"
    debug "fix la $filename $prefix"
    if [ "$prefix" ]; then
        pkg_run sed -i "s|^\(libdir=\)'\(.*\)'$|\1'${prefix}\2'|" "$filename"
    fi
}

pkg_link() {
    local dst="${1:?}" src="${2:?}"

    pkg_run cd $(dirname "$dst")
    pkg_run rm -rf "$src"
    pkg_run ln -rs $(basename "$dst") "$src"
    pkg_run cd "$OLDPWD"
}

pkg_cmake_use_chicken() {
    local S="$jagen_FS" A=

    A="$A$S-DCHICKEN_COMPILER=$jagen_host_dir/bin/chicken"
    A="$A$S-DCHICKEN_INTERPRETER=$jagen_host_dir/bin/csi"

    printf "$A"
}

jagen_pkg_unpack_pre() {
    cd "$jagen_build_dir"
}

default_unpack() {
    set -- $pkg_source
    local src_type="$1"
    local src_path="$2"

    : ${pkg_name:?}
    : ${pkg_work_dir:?}

    pkg_run rm -rf "$pkg_work_dir"

    case $src_type in
        git|hg|repo)
            if [ -d "${pkg_source_dir:?}" ]; then
                if in_list "$pkg_name" $jagen_source_exclude; then
                    message "not cleaning $pkg_name: excluded"
                else
                    _jagen src clean "$pkg_name"  || return
                fi

                if in_flags offline; then
                    message "not updating $pkg_name: offline mode"
                else
                    _jagen src update "$pkg_name" || return
                fi
            else
                if in_flags offline; then
                    die "could not clone $pkg_name in offline mode"
                else
                    _jagen src clone "$pkg_name"
                fi
            fi
            ;;
        dist)
            pkg_run mkdir -p "$pkg_work_dir"
            pkg_run tar -C "$pkg_work_dir" -xf "$src_path"
            ;;
        *)
            if [ "$pkg_source_dir" ]; then
                pkg_run mkdir -p "$pkg_source_dir"
            fi
            ;;
    esac
}

jagen_pkg_unpack() {
    default_unpack
}

jagen_pkg_patch_pre() {
    [ "$pkg_source_dir" ] || return 0
    pkg_run cd "$pkg_source_dir"
}

default_patch() {
    if is_function jagen_pkg_apply_patches; then
        jagen_pkg_apply_patches
    fi
}

jagen_pkg_patch() {
    default_patch
}

jagen_pkg_autoreconf() {
    [ "$pkg_source_dir" ] || return 0
    pkg_run cd "$pkg_source_dir"
    if [ "$pkg_build_generate" ]; then
        if [ -x ./autogen.sh ]; then
            pkg_run ./autogen.sh
        fi
    else
        pkg_run autoreconf -if -I "$jagen_host_dir/share/aclocal"
    fi
}

default_build() {
    local OLDIFS="$IFS" IFS="$jagen_IFS" S="$jagen_FS" A=

    [ "$pkg_source_dir" ] || return 0

    case $pkg_build_type in
        GNU)
            if [ -x "$pkg_source_dir/configure" ]; then
                pkg_run "$pkg_source_dir/configure" \
                    --host="$pkg_system" \
                    --prefix="$pkg_prefix" \
                    --disable-dependency-tracking \
                    $pkg_options "$@"

                IFS=$OLDIFS
                pkg_run make
            else
                die "GNU build type specified but ./configure was not found in $pkg_source_dir"
            fi
            ;;
        CMake)
            if [ "$jagen_cmake_module_path" ]; then
                A="$A$S-DCMAKE_MODULE_PATH=$jagen_cmake_module_path"
            fi
            if [ "$pkg_config" = "target" ]; then
                A="$A$S-DCMAKE_SYSTEM_NAME=Linux"
                A="$A$S-DCMAKE_C_COMPILER=${jagen_toolchain_prefix}gcc"
                A="$A$S-DCMAKE_FIND_ROOT_PATH=$pkg_install_dir"
            fi

            # NOTE: CMakePackageConfigHelpers.cmake does not handle empty
            # CMAKE_INSTALL_PREFIX, setting it to '/' should have the same
            # effect.
            pkg_run cmake -G"${jagen_cmake_generator:?}" \
                -DCMAKE_BUILD_TYPE="$jagen_cmake_build_type" \
                -DCMAKE_INSTALL_PREFIX="${pkg_prefix:-/}" \
                $A $jagen_cmake_options "$@" "$pkg_source_dir"

            IFS=$OLDIFS
            pkg_run cmake --build . -- $jagen_cmake_build_options
            ;;
    esac
}

jagen_pkg_build() {
    default_build
}

default_install() {
    case $pkg_build_type in
        GNU)
            pkg_run make DESTDIR="$pkg_dest_dir" install

            for name in $pkg_libs; do
                pkg_fix_la "$pkg_dest_dir$pkg_prefix/lib/lib${name}.la" "$pkg_dest_dir"
            done
            ;;
        CMake)
            export DESTDIR="$pkg_dest_dir"
            pkg_run cmake --build . --target install
            ;;
    esac
}

jagen_pkg_install() {
    default_install
}
