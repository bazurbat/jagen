#!/bin/sh

. "$jagen_dir/env.sh" ||
    { echo "Failed to load environment"; exit 1; }

# Do not prompt on the terminal (e.g. when asking for HTTP credentials).
export GIT_TERMINAL_PROMPT=0
# Never install the translations.
export LINGUAS=""

: ${pkg_run_on_error:=exit}
: ${jagen_cmake_generator:=Ninja}

pkg_run() {
    local IFS; unset IFS
    local cmd="$1" jobs="${pkg_build_jobs:-$(jagen_nproc)}"
    shift

    case $cmd in
        make)
            cmd="$cmd -j$jobs"
            [ "$jagen_build_verbose" ] && cmd="$cmd V=1"
            ;;
        ninja)
            cmd="$cmd -j$jobs"
            [ "$jagen_build_verbose" ] && cmd="$cmd -v"
            ;;
    esac

    debug $cmd "$*"
    $cmd "$@" || $pkg_run_on_error
}

pkg_run_patch() {
    local num="${1:?}" filename="${2:?}"
    pkg_run patch -p"$num" -i "$filename"
}

pkg_strip_root() {
    local root="${1:?}" files
    local strip="${pkg_toolchain_prefix}strip"

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
    debug1 "fix pc $filename"
    if [ -f "$filename" -a "$pkg_install_root" ]; then
        pkg_run sed -i "s|$pkg_install_root||g" "$filename"
    fi
}

pkg_fix_la() {
    local filename="${1:?}" prefix="$2"
    debug1 "fix la $filename $prefix"
    if [ "$prefix" ]; then
        pkg_run sed -i "s|^\(libdir=\)'\(.*\)'$|\1'${prefix}\2'|" "$filename"
    fi
}

pkg_fix_config_script() {
    local filename="${1:?}"
    if [ "$pkg_install_root" -a -f "$filename" ]; then
        pkg_run sed -ri "s|^(prefix=)$pkg_install_prefix$|\1$pkg_install_root|" $filename
    fi
}

pkg_run_ldconfig() {
    pkg_run ldconfig -n "$pkg_install_dir/lib"
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
    local dir="$(dirname "$src")"

    pkg_run mkdir -p "$dir"
    pkg_run cd "$dir"
    pkg_run rm -rf "$(basename "$src")"
    pkg_run ln -rs "$target" "$src"
    pkg_run cd "$OLDPWD"
}

pkg__get_cmake_args() {
    local args= v_arg= j_arg=
    if [ "$jagen_build_verbose" ]; then
        case $pkg_build_generator in
            *Ninja)     v_arg="-v"        ;;
            *Makefiles) v_arg="VERBOSE=1" ;;
        esac
    fi
    case $pkg_build_generator in
        *Makefiles) j_arg=${pkg_build_jobs:-$(jagen_nproc)} ;;
    esac
    args="$v_arg $j_arg"; args=${args# }; args=${args% }
    printf '%s' "$args"
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

pkg__path_is_uri() {
    [ "${1:?}" != "${1#*://}" ]
}

pkg__uri_is_local() {
    [ "${1:?}" != "${1#file://}" ]
}

pkg__unpack_dist() {
    local src_path="${1:?}"
    local work_dir="${2:?}"
    local dist_path="${jagen_dist_dir:?}/${pkg_source_filename:?}"

    if ! [ -f "$dist_path" ]; then
        if pkg__path_is_uri "$src_path"; then
            if in_flags offline && ! pkg__uri_is_local "$src_path"; then
                die "unable to download $src_path: offline mode"
            else
                pkg__download "$src_path" "$dist_path"
            fi
        else
            die "unable to unpack $dist_path: the file is not found and download location is not specified"
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
    pkg_run cd "$work_dir"

    case $pkg_source_filename in
        *.tar|*.tar.*|*.tgz|*.tbz2|*.txz)
            pkg_run tar -xf "$dist_path" ;;
        *.zip)
            # to emulate the tar convention on putting the directory name
            # inside the archive
            pkg_run unzip -d "$pkg_source_basename" "$dist_path" ;;
        *)
            die "unable to unpack '$pkg_source_filename': unknown archive format" ;;
    esac
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
            if [ -d "$pkg_source_dir" ] &&
               ! jagen__is_empty "$pkg_source_dir" &&
               [ "$pkg_source_exclude" ]
            then
                message "not updating $pkg_name: the source is excluded"
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
    esac
}

pkg_patch() {
    local IFS; unset IFS
    if is_function jagen_pkg_apply_patches; then
        if [ "$pkg_source_exclude" ]; then
            message "not patching $pkg_name: the source is excluded"
        else
            jagen_pkg_apply_patches
        fi
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
    [ "$pkg_source_dir" ] || return 0

    local IFS="$jagen_IFS" S="$jagen_FS" A=

    case $pkg_build_type in
        GNU)
            if [ "$pkg_install_root" ]; then
                LDFLAGS="$LDFLAGS -Wl,-rpath-link=$pkg_install_dir/lib"
            fi

            if [ "$pkg_build_configure_needs_install_dir" ]; then
                CFLAGS="$CFLAGS -I$pkg_install_dir/include"
                LDFLAGS="$LDFLAGS -L$pkg_install_dir/lib"
            fi

            if pkg_is_debug; then
                CFLAGS="$CFLAGS -g -O0"
            elif pkg_is_release_with_debug; then
                CFLAGS="$CFLAGS -g"
            fi

            export CFLAGS LDFLAGS

            pkg_run "${pkg_build_configure_file:-$pkg_source_dir/configure}" $A \
                ${pkg_build_system:+--host="$pkg_build_system"} \
                --prefix="$pkg_install_prefix" \
                --disable-dependency-tracking \
                ${pkg_install_root:+--with-sysroot="$pkg_install_root"} \
                $pkg_build_options "$@"

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

            if [ "$pkg_build_cmake_module_path" ]; then
                A="$A$S-DCMAKE_MODULE_PATH=$pkg_build_cmake_module_path"
            fi

            if [ "$pkg_build_cmake_toolchain_file" ]; then
                A="$A$S-DCMAKE_TOOLCHAIN_FILE=$pkg_build_cmake_toolchain_file"
            fi

            # Command line assignments create cache entries, so they can not
            # override settings from the toolchain file.
            if [ "$pkg_config" = "target" ]; then
                A="$A$S-DCMAKE_SYSTEM_NAME=Linux"
                A="$A$S-DCMAKE_FIND_ROOT_PATH=$pkg_install_dir"
                if [ -z "$CC" ]; then
                    A="$A$S-DCMAKE_C_COMPILER=${pkg_toolchain_prefix}gcc"
                fi
                if [ -z "$CXX" ]; then
                    A="$A$S-DCMAKE_CXX_COMPILER=${pkg_toolchain_prefix}g++"
                fi
            fi

            if pkg_is_debug; then
                # assuming that global defaults are for 'release' config
                unset CFLAGS CXXFLAGS
                A="$A$S-DCMAKE_C_FLAGS="
                A="$A$S-DCMAKE_CXX_FLAGS="
            fi

            # Remove CMake's defaults which are appended to the generic flags
            # and override our environment.
            A="$A$S-DCMAKE_C_FLAGS_RELEASE="
            A="$A$S-DCMAKE_CXX_FLAGS_RELEASE="

            if $(jagen__versions ge "$(jagen__get_cmake_version)" 3.1); then
                A="$A$S-DCMAKE_EXPORT_NO_PACKAGE_REGISTRY=YES"
                A="$A$S-DCMAKE_FIND_PACKAGE_NO_PACKAGE_REGISTRY=YES"
                A="$A$S-DCMAKE_FIND_PACKAGE_NO_SYSTEM_PACKAGE_REGISTRY=YES"
            fi

            pkg_run cmake -G"$pkg_build_generator" \
                -DCMAKE_BUILD_TYPE="$(pkg_cmake_build_type)" \
                -DCMAKE_INSTALL_PREFIX="$pkg_install_prefix" \
                $A $jagen_cmake_options $pkg_build_options "$@" "$pkg_source_dir"
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

    local is_offline= verbose_opt= jobs=

    if in_flags offline; then
        is_offline=1
    fi

    case $pkg_build_type in
        GNU)
            pkg_run make "$@"
            ;;
        CMake)
            pkg_run cmake --build . -- $(pkg__get_cmake_args) "$@"
            ;;
        make|kbuild)
            pkg_run make $pkg_build_options "$@"
            ;;
        linux_kernel)
            use_env kbuild
            pkg_run cd "$pkg_source_dir"
            pkg_run make "${jagen_kernel_image:?}" modules
            ;;
        linux_module)
            pkg_run make $pkg_build_options "$@"
            ;;
    esac
}

pkg_install() {
    [ "$pkg_source_dir" ] || return 0

    local IFS="$jagen_IFS"
    local pkg_install_type="${pkg_install_type:-$pkg_build_type}"

    case $pkg_install_type in
        GNU|make|kbuild)
            pkg_run make \
                ${pkg_install_root:+DESTDIR="$pkg_install_root"} \
                $pkg_install_args "$@" install

            for name in $pkg_install_libs; do
                pkg_fix_pc "$name"
                # pkg_fix_la "$pkg_install_root$pkg_install_prefix/lib/lib${name}.la" "$pkg_install_root"

                if [ -z "$pkg_install_config_script" ]; then
                    pkg_install_config_script="/bin/${pkg_name}-config"
                fi
                pkg_fix_config_script "${pkg_install_dir:?}${pkg_install_config_script}"
            done
            ;;
        CMake)
            if [ "$pkg_install_root" ]; then
                export DESTDIR="$pkg_install_root"
            fi
            pkg_run cmake --build . --target install -- \
                $(pkg__get_cmake_args) $pkg_install_args "$@"
            unset DESTDIR
            ;;
        linux_kernel)
            use_env kbuild
            pkg_run cd "$pkg_source_dir"
            pkg_run install -vm644 \
                "$pkg_build_dir/arch/$pkg_build_arch/boot/$jagen_kernel_image" \
                "$jagen_build_dir"
            pkg_run make \
                INSTALL_MOD_PATH="${INSTALL_MOD_PATH:-${pkg_install_dir:?}}" \
                $pkg_install_args "$@" modules_install
            ;;
        linux_module)
            pkg_install_modules
            ;;
        toolchain)
            require toolchain
            toolchain_generate_wrappers    \
                "${jagen_bin_dir:?}"       \
                "${pkg_source_dir:?}/bin"  \
                "${pkg_toolchain_prefix}"
            ;;
        none)
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

pkg_install_file() {
    local src="$(find_in_path "${1:?}")" dest="${2:?}"
    [ -f "$src" ] || die "failed to find '$1' in path"
    pkg_run mkdir -p "$(dirname "$dest")"
    pkg_run cp -vf "$src" "$dest"
}

pkg__image() {
    case $pkg_build_type in
        linux_kernel)
            use_env kbuild
            pkg_run cd "$pkg_source_dir"
            pkg_run make "${jagen_kernel_image:?}"
            pkg_run install -vm644 \
                "$pkg_build_dir/arch/$pkg_build_arch/boot/$jagen_kernel_image" \
                "$jagen_build_dir"
            ;;
    esac
}

# stages

jagen_pkg_unpack() {
    local source_dir= target_dir= is_toolchain=
    local work_dir="$pkg_work_dir" pkg_work_dir="$pkg_work_dir"

    pkg_run rm -rf "${pkg_work_dir:?}"

    # check for source is required for system toolchains such as gcc-native
    # which are not unpacked but linked from ther location directly
    if [ "$pkg_source" ] && [ "$pkg_source_dir" ] &&
       [ "$pkg_install_type" = 'toolchain' ] &&
       [ "$jagen_toolchains_dir" ]
    then
        is_toolchain=1
        # in the case source.dir is redefined
        source_dir=$(basename "$pkg_source_dir"); : ${source_dir:?}
        target_dir="$jagen_toolchains_dir/$source_dir"
        pkg_work_dir="$jagen_toolchains_dir"
    fi

    if [ -z "$target_dir" ] || ! [ -d "$target_dir" ]; then
        pkg_unpack
    fi

    if [ "$is_toolchain" ]; then
        if [ -d "$target_dir" ]; then
            pkg_run mkdir -p "$work_dir"
            pkg_link "$target_dir" "$work_dir/$source_dir"
        else
            die "expected to find '$pkg_name' toolchain in shared toolchains"\
                "dir but the directory '$target_dir' does not exist"
        fi
    fi
}

jagen_pkg_patch() {
    pkg_patch
}

jagen_pkg_provide_patches() {
    local IFS="$jagen_IFS"
    for filename in ${pkg_patches_provided-}; do
        if [ -f "$filename" ]; then
            if ! [ -s "$filename" ]; then
                die "providing a patch '$filename' but the file is empty"
            fi
        else
            die "must provide a patch '$filename' but the file does not exist"
        fi
    done
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
