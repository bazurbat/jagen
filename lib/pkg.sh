#!/bin/sh

. "$jagen_dir/lib/main.sh" ||
    { echo "Failed to load environment"; exit 1; }

: ${pkg_run_jobs:=$(jagen_nproc)}
: ${pkg_run_on_error:=exit}
: ${jagen_cmake_generator:=Ninja}

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

pkg_run_patch() {
    local num="${1:?}" name="${2:?}"
    pkg_run patch -p$num -i "$jagen_dist_dir/patches/${name}.patch"
}

pkg_strip_root() {
    local root="${1:?}" files
    local strip="${jagen_toolchain_prefix:?}strip"

    files=$(find "$root" -type f -not -name "*.ko" \
        "(" -path "*/lib*" -o -path "*/bin*" -o -path "*/sbin*" ")" | \
        xargs -r file | grep "ELF.*\(executable\|shared object\).*not stripped" | cut -d: -f1)

    for f in $files; do
        pkg_run "$strip" --strip-unneeded \
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

# Some packages write full paths with sysroot prepended to their pc files which
# causes the sysroot to be prepended twice in build flags of packages which
# actually support it. Namely fontconfig does this. It is easier to patch
# everything just in case than fixing every individual package.
pkg_fix_pc() {
    local name="${1:?}"
    local filename="$pkg_install_dir/lib/pkgconfig/${name}.pc"
    debug "fix pc $filename"
    if [ -f "$filename" ]; then
        pkg_run sed -i "s|$pkg_sysroot||g" "$filename"
    fi
}

pkg_fix_la() {
    local filename="${1:?}" prefix="$2"
    debug "fix la $filename $prefix"
    if [ "$prefix" ]; then
        pkg_run sed -i "s|^\(libdir=\)'\(.*\)'$|\1'${prefix}\2'|" "$filename"
    fi
}

pkg_fix_config_script() {
    local filename="${1:?}"
    if [ "$pkg_sysroot" -a -f "$filename" ]; then
        pkg_run sed -ri "s|^(prefix=)$pkg_prefix$|\1$pkg_sysroot|" $filename
    fi
}

pkg_link() {
    local src="${1:?}" dst="${2:?}"

    pkg_run cd $(dirname "$dst")
    pkg_run rm -rf $(basename "$dst")
    pkg_run ln -rs "$src" "$dst"
    pkg_run cd "$OLDPWD"
}

pkg_get_build_profile() {
    local profile="${pkg_build_profile:-$jagen_build_profile}"
    case $profile in
        release|debug|release_with_debug)
            echo $profile ;;
        *)
            echo release ;;
    esac
}

pkg_cmake_build_type() {
    local profile="$(pkg_get_build_profile)"
    case $profile in
        release)
            echo "Release" ;;
        debug)
            echo "Debug" ;;
        release_with_debug)
            echo "RelWithDebInfo" ;;
        *)
            echo "Release" ;;
    esac
}

pkg_is_release() {
    test "$(pkg_get_build_profile)" = "release"
}

pkg_is_debug() {
    test "$(pkg_get_build_profile)" = "debug"
}

pkg_is_release_with_debug() {
    test "$(pkg_get_build_profile)" = "release_with_debug"
}

# usings

pkg_using_install_prefix() {
    local prefix="${1:-${pkg_prefix:?}}"
    printf '%s' "-DCMAKE_INSTALL_PREFIX=$prefix"
}

pkg_using_host_chicken() {
    local S="$jagen_FS" A=
    A="$A$S-DCHICKEN_COMPILER=$jagen_host_dir/bin/chicken"
    A="$A$S-DCHICKEN_INTERPRETER=$jagen_host_dir/bin/csi"
    printf '%s' "$A"
}

pkg_using_target_board() {
    printf '%s' "-DTARGET_BOARD=${jagen_target_board:?}"
}

# default stages

pkg_unpack() {
    set -- $pkg_source
    local src_type="$1"
    local src_path="$2"

    : ${pkg_name:?}
    : ${pkg_work_dir:?}

    pkg_run rm -rf "$pkg_work_dir"

    case $src_type in
        git|hg|repo)
            if [ -d "$pkg_source_dir" ]; then
                if [ "$pkg_source_exclude" ] ||
                    in_list "$pkg_name" $jagen_source_exclude
                then
                    message "not updating $pkg_name: excluded"
                else
                    _jagen src clean "$pkg_name"  || return
                    _jagen src update "$pkg_name" || return
                fi
            else
                _jagen src update "$pkg_name" || return
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

pkg_patch() {
    if is_function jagen_pkg_apply_patches; then
        jagen_pkg_apply_patches
    fi
}

pkg_autoreconf() {
    [ "$pkg_source_dir" ] || return 0
    pkg_run cd "$pkg_source_dir"
    if [ "$pkg_build_generate" ]; then
        if [ -x ./autogen.sh ]; then
            pkg_run ./autogen.sh
        fi
    else
        pkg_run autoreconf -vif -I "$jagen_host_dir/share/aclocal"
    fi
}

pkg_configure() {
    local IFS="$jagen_IFS" S="$jagen_FS" A=

    [ "$pkg_source_dir" ] || return 0

    case $pkg_build_type in
        GNU)
            if ! [ -x "$pkg_source_dir/configure" ]; then
                die "GNU build type specified but no ./configure was found in $pkg_source_dir"
            fi

            if [ "$pkg_sysroot" ]; then
                LDFLAGS="$LDFLAGS -Wl,-rpath-link=$pkg_install_dir/lib"
            fi

            if [ "$pkg_configure_needs_install_dir" ]; then
                CFLAGS="$CFLAGS -I$pkg_install_dir/include"
                LDFLAGS="$LDFLAGS -L$pkg_install_dir/lib"
            fi

            export CFLAGS LDFLAGS

            pkg_run "$pkg_source_dir/configure" $A \
                ${pkg_system:+--host="$pkg_system"} \
                --prefix="$pkg_prefix" \
                --disable-dependency-tracking \
                ${pkg_sysroot:+--with-sysroot="$pkg_sysroot"} \
                $pkg_options "$@"

            # Never add RPATH to generated binaries because libtool uses
            # various heuristics to determine when to add it, some Linux
            # distributions patch it to adjust a list of 'system' paths, but
            # generally things seems to work because everyone install to
            # /usr/local/lib or /usr/lib or lib64 whatever and these are
            # handled specially. Embedded systems often have different
            # conventions and naming schemes, libtool not always does the
            # 'right' thing and you might end up with a mixed bag of libraries
            # some having RPATH and some not.

            if [ -x ./libtool ]; then
                pkg_run sed -i 's|\(hardcode_into_libs\)=yes|\1=no|g' \
                    "./libtool"
            fi

            ;;
        CMake)
            if ! [ -f "$pkg_source_dir/CMakeLists.txt" ]; then
                die "CMake build type specified but no CMakeLists.txt was found in $pkg_source_dir"
            fi

            if [ "$jagen_cmake_module_path" ]; then
                A="$A$S-DCMAKE_MODULE_PATH=$jagen_cmake_module_path"
            fi
            if [ "$pkg_config" = "target" ]; then
                A="$A$S-DCMAKE_SYSTEM_NAME=Linux"
                A="$A$S-DCMAKE_C_COMPILER=${jagen_toolchain_prefix}gcc"
                A="$A$S-DCMAKE_FIND_ROOT_PATH=$pkg_install_dir"
            fi

            pkg_run cmake -G"${jagen_cmake_generator:?}" \
                -DCMAKE_BUILD_TYPE="$(pkg_cmake_build_type)" \
                -DCMAKE_INSTALL_PREFIX="$pkg_install_dir" \
                $A $jagen_cmake_options "$@" "$pkg_source_dir"
            ;;
        skarnet)
            pkg_run ./configure \
                ${pkg_system:+--host="$pkg_system"} \
                ${pkg_prefix:+--prefix="$pkg_prefix"} \
                ${jagen_sysdeps_cfg:+--with-sysdeps="$jagen_sysdeps_cfg"} \
                --with-include="$pkg_install_dir/include" \
                --with-dynlib="$pkg_install_dir/lib" \
                "$@"
            ;;
        *)
            ;;
    esac
}

pkg_compile() {
    [ "$pkg_source_dir" ] || return 0

    case $pkg_build_type in
        GNU|make|KBuild|skarnet)
            pkg_run make "$@"
            ;;
        CMake)
            pkg_run cmake --build . -- $jagen_cmake_build_options "$@"
            ;;
        linux_module)
            pkg_run make "$@" modules
            ;;
    esac
}

pkg_install() {
    : ${pkg_install_dir:?}

    case $pkg_build_type in
        GNU|make|skarnet)
            pkg_run make ${pkg_sysroot:+DESTDIR="$pkg_sysroot"} "$@" install

            for name in $pkg_libs; do
                pkg_fix_pc "$name"
                # pkg_fix_la "$pkg_sysroot$pkg_prefix/lib/lib${name}.la" "$pkg_sysroot"

                if [ -z "$pkg_install_config_script" ]; then
                    pkg_install_config_script="/bin/${pkg_name}-config"
                fi
                pkg_fix_config_script "${pkg_install_dir}${pkg_install_config_script}"
            done
            ;;
        CMake)
            pkg_run cmake --build . --target install -- "$@"
            ;;
        linux_module)
            pkg_run make INSTALL_MOD_PATH="$pkg_install_dir" "$@" modules_install
            ;;
    esac
}

# stages

jagen_pkg_unpack() {
    pkg_unpack
}

jagen_pkg_patch() {
    pkg_patch
}

jagen_pkg_autoreconf() {
    pkg_autoreconf
}

jagen_pkg_configure() {
    pkg_configure
}

jagen_pkg_compile() {
    pkg_compile
}

jagen_pkg_install() {
    pkg_install
}
