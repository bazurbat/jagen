#!/bin/sh

. "$jagen_dir/env.sh" ||
    { echo "Failed to load environment"; exit 1; }

: ${pkg_run_jobs:=$(jagen_nproc)}
: ${pkg_run_on_error:=exit}
: ${jagen_cmake_generator:=Ninja}

pkg_run() {
    local IFS; unset IFS
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
    pkg_run patch -p$num -i "${jagen_dist_patches_dir:?}/${name}.patch"
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

# Some packages write full paths with sysroot prepended to their pc files which
# causes the sysroot to be prepended twice in build flags of packages which
# actually support it. Namely fontconfig does this. It is easier to patch
# everything just in case than fixing every individual package.
pkg_fix_pc() {
    local name="${1:?}"
    local filename="$pkg_install_dir/lib/pkgconfig/${name}.pc"
    debug "fix pc $filename"
    if [ -f "$filename" -a "$pkg_sysroot" ]; then
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

pkg_sync_dirs() {
    local source_dir="${1:?}"
    local dest_dir="${2:?}"
    local filter_file=

    [ -d "$source_dir" ] ||
        die "Sync source directory '$source_dir' is not exists"
    [ -d "$dest_dir" ] ||
        die "Sync destination directory '$dest_dir' is not exists"

    if [ "$3" ]; then
        filter_file=$(find_in_path "$3")
        [ "$filter_file" ] ||
            die "Could not find filter file '$3' for syncronization of '$source_dir' to '$dest_dir'"
    fi

    pkg_run rsync -va --delete --delete-excluded \
        ${filter_file:+--filter="merge ${filter_file}"} \
        "$source_dir" "$dest_dir"
}

pkg_link() {
    local target="${1:?}" src="${2:?}"

    pkg_run cd "$(dirname "$src")"
    pkg_run rm -rf "$(basename "$src")"
    pkg_run ln -rs "$target" "$src"
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

pkg_using_host_chicken() {
    local S="$jagen_FS" A=
    A="$A$S-DCHICKEN_COMPILER=$jagen_host_dir/bin/chicken"
    A="$A$S-DCHICKEN_INTERPRETER=$jagen_host_dir/bin/csi"
    A="$A$S-DCHICKEN_SYSROOT=$jagen_target_dir"
    printf '%s' "$A"
}

pkg_using_target_board() {
    printf '%s' "-DTARGET_BOARD=${jagen_target_board:-unknown}"
}

# default stages

pkg__on_download_exit() {
   if [ "$pkg__current_download" ]; then
       pkg_run rm -f "$pkg__current_download"
   fi
}

pkg__download() {
    local src_path="${1:?}"
    local dest_path="${2:?}"

    pkg__current_download="$dest_path"

    trap pkg__on_download_exit EXIT

    pkg_run mkdir -p "${dest_path%/*}"
    curl -L "$src_path" -o "$dest_path" ||
        die "failed to download $src_path"

    trap - EXIT
}

pkg__unpack_dist() {
    local src_path="${1:?}"
    local work_dir="${2:?}"
    local dist_path="${jagen_dist_dir:?}/${pkg_source_filename:?}"

    if ! [ -f "$dist_path" ]; then
        if in_flags offline; then
            die "could not download required source for $pkg_name in offline mode"
        else
            pkg__download "$src_path" "$dist_path"
        fi
    fi

    if [ "$pkg_source_sha256sum" ]; then
        if [ "$(command -v sha256sum)" ]; then
            echo "$pkg_source_sha256sum $dist_path" | sha256sum -c - ||
                die "failed to verify sha256sum of $dist_path"
        else
            warning "sha256sum is not found in PATH, can not verify $pkg_name"
        fi
    elif [ "$pkg_source_sha1sum" ]; then
        if [ "$(command -v sha1sum)" ]; then
            echo "$pkg_source_sha1sum $dist_path" | sha1sum -c - ||
                die "failed to verify sha1sum of $dist_path"
        else
            warning "sha1sum is not found in PATH, can not verify $pkg_name"
        fi
    elif [ "$pkg_source_md5sum" ]; then
        if [ "$(command -v md5sum)" ]; then
            echo "$pkg_source_md5sum $dist_path" | md5sum -c - ||
                die "failed to verify md5sum of $dist_path"
        else
            warning "md5sum is not found in PATH, can not verify $pkg_name"
        fi
    fi

    [ -f "$dist_path" ] ||
        die "could not find $dist_path for unpacking"

    pkg_run mkdir -p "$work_dir"
    pkg_run tar -C "$work_dir" -xf "$dist_path"
}

pkg_unpack() {
    local IFS; unset IFS
    set -- $pkg_source
    local src_type="$1"
    local src_path="$2"

    case $src_type in
        dist)
            pkg__unpack_dist "$src_path" "$pkg_work_dir"
            ;;
        git|hg|repo)
            if [ -d "$pkg_source_dir" ] && ! jagen__is_empty "$pkg_source_dir"; then
                if [ "$pkg_source_exclude" ] ||
                    in_list "$pkg_name" $jagen_source_exclude
                then
                    message "skipping unpack of $pkg_name: excluded by configuration"
                elif _jagen src dirty "$pkg_name" &&
                    ! is_function jagen_pkg_apply_patches
                then
                    message "skipping unpack of $pkg_name: the source directory '$pkg_source_dir' is dirty"
                else
                    _jagen src clean "$pkg_name"  || return
                    _jagen src update "$pkg_name" || return
                fi
            else
                _jagen src update "$pkg_name" || return
            fi
            ;;
        curl)
            local install_dir="${pkg_install_dir:-$jagen_host_dir}/bin"
            local dest="$install_dir/$pkg_name"
            pkg_run mkdir -p "$install_dir"
            curl -L "$src_path" > "$dest" ||
                die "failed to download $pkg_name from $src_path"
            pkg_run chmod +x "$dest"
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
        if [ -f ./autogen.sh ]; then
            pkg_run sh ./autogen.sh
        fi
    else
        pkg_run mkdir -p m4
        pkg_run autoreconf -vif -I "$jagen_host_dir/share/aclocal"
    fi
}

pkg_configure() {
    local IFS="$jagen_IFS" S="$jagen_FS" A=

    [ "$pkg_source_dir" ] || return 0

    case $pkg_build_type in
        GNU)
            if [ "$pkg_sysroot" ]; then
                LDFLAGS="$LDFLAGS -Wl,-rpath-link=$pkg_install_dir/lib"
            fi

            if [ "$pkg_configure_needs_install_dir" ]; then
                CFLAGS="$CFLAGS -I$pkg_install_dir/include"
                LDFLAGS="$LDFLAGS -L$pkg_install_dir/lib"
            fi

            if pkg_is_debug; then
                CFLAGS="$CFLAGS -g -O0"
            elif pkg_is_release_with_debug; then
                CFLAGS="$CFLAGS -g"
            fi

            export CFLAGS LDFLAGS

            pkg_run "${pkg_configure_file:-$pkg_source_dir/configure}" $A \
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
                A="$A$S-DCMAKE_CXX_COMPILER=${jagen_toolchain_prefix}g++"
                A="$A$S-DCMAKE_FIND_ROOT_PATH=$pkg_install_dir"
            fi

            if $(jagen__versions ge "$(jagen__get_cmake_version)" 3.1); then
                A="$A$S-DCMAKE_EXPORT_NO_PACKAGE_REGISTRY=YES"
                A="$A$S-DCMAKE_FIND_PACKAGE_NO_PACKAGE_REGISTRY=YES"
                A="$A$S-DCMAKE_FIND_PACKAGE_NO_SYSTEM_PACKAGE_REGISTRY=YES"
            fi

            pkg_run cmake -G"${pkg_build_generator:-${jagen_cmake_generator:?}}" \
                -DCMAKE_BUILD_TYPE="$(pkg_cmake_build_type)" \
                -DCMAKE_INSTALL_PREFIX="$pkg_prefix" \
                $A $jagen_cmake_options $pkg_options "$@" "$pkg_source_dir"
            ;;
        linux_kernel)
            use_env kbuild
            pkg_run cd "$pkg_source_dir"
            pkg_run make "${jagen_kernel_config:?}"
            pkg_run make prepare
            ;;
        *)
            ;;
    esac
}

pkg_compile() {
    [ "$pkg_source_dir" ] || return 0

    local is_offline

    if in_flags offline; then
        is_offline=1
    fi

    case $pkg_build_type in
        GNU|KBuild)
            pkg_run make "$@"
            ;;
        CMake)
            pkg_run cmake --build . -- $jagen_cmake_build_options "$@"
            ;;
        make)
            pkg_run make $pkg_options "$@"
            ;;
        linux_kernel)
            use_env kbuild
            pkg_run cd "$pkg_source_dir"
            pkg_run make "${jagen_kernel_image:?}" modules
            ;;
        linux_module)
            pkg_run make $pkg_options "$@"
            ;;
        maven)
            pkg_run "${jagen_mvn_exe:-mvn}" \
                ${is_offline:+-o} \
                $jagen_mvn_options $pkg_options "$@" \
                verify
            ;;
    esac
}

pkg_install() {
    local pkg_install_type="${pkg_install_type:-$pkg_build_type}"

    case $pkg_install_type in
        GNU|make)
            pkg_run make ${pkg_sysroot:+DESTDIR="$pkg_sysroot"} "$@" install

            for name in $pkg_libs; do
                pkg_fix_pc "$name"
                # pkg_fix_la "$pkg_sysroot$pkg_prefix/lib/lib${name}.la" "$pkg_sysroot"

                if [ -z "$pkg_install_config_script" ]; then
                    pkg_install_config_script="/bin/${pkg_name}-config"
                fi
                pkg_fix_config_script "${pkg_install_dir:?}${pkg_install_config_script}"
            done
            ;;
        CMake)
            if [ "$pkg_sysroot" ]; then
                export DESTDIR="$pkg_sysroot"
            fi
            pkg_run cmake --build . --target install -- "$@"
            unset DESTDIR
            ;;
        linux_kernel)
            use_env kbuild
            pkg_run cd "$pkg_source_dir"
            pkg_run install -vm644 \
                "$pkg_build_dir/arch/$jagen_target_arch/boot/$jagen_kernel_image" \
                "$jagen_build_dir"
            pkg_run make \
                INSTALL_MOD_PATH="${INSTALL_MOD_PATH:-${pkg_install_dir:?}}" \
                "$@" modules_install
            ;;
        linux_module)
            pkg_install_modules
            ;;
        none)
            ;;
    esac
}

pkg__modules_install() {
    pkg_run make -C "${KERNEL_SRC:?}" M="$PWD/$1" \
        INSTALL_MOD_PATH="${INSTALL_MOD_PATH:-${pkg_install_dir:?}}" \
        modules_install
}

pkg_install_modules() {
    local dir

    if [ "$pkg_install_modules_dirs" ]; then
        for dir in $pkg_install_modules_dirs; do
            pkg__modules_install "$dir"
        done
    else
        # NOTE: Passing '.' here causes 'modules_install' to append
        # full path to the module sources after install-dir.
        pkg__modules_install ''
    fi
}

pkg__image() {
    case $pkg_build_type in
        linux_kernel)
            use_env kbuild
            pkg_run cd "$pkg_source_dir"
            pkg_run make "${jagen_kernel_image:?}"
            pkg_run install -vm644 \
                "$pkg_build_dir/arch/$jagen_target_arch/boot/$jagen_kernel_image" \
                "$jagen_build_dir"
            ;;
    esac
}

# stages

jagen_pkg_unpack() {
    pkg_run rm -rf "${pkg_work_dir:?}"
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

jagen_pkg_image() {
    pkg__image
}
