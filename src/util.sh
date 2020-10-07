#!/bin/sh

pkg__get_jobs() {
    printf "%s" "${pkg_build_jobs:-${jagen_jobs:-$(jagen_nproc)}}"
}

pkg_run() {
    local IFS; unset IFS
    local cmd="$1" jobs="$(pkg__get_jobs)"
    shift

    jagen__need_cmd "$cmd"

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

    debug1 $cmd "$*"
    $cmd "$@" || exit
}

pkg__rm() {
    local item
    for item in "$@"; do
        if [ "$item" ]; then
            debug "removing $item"
            rm -f "$item"
        fi
    done
}

pkg__curl() {
    jagen__need_cmd curl
    pkg_run curl -fL ${jagen_insecure:+--insecure} "$@"
}

pkg_run_patch() {
    local num="${1:?}" filename="${2:?}" out err
    message "applying patch '$filename' ($num)"
    debug1 patch -r- -N -p"$num" -i "$filename"
    out=$(LC_LANG=C patch -r- -N -p"$num" -i "$filename"); err=$?
    # print the captured output to make it visible in the logs
    echo "$out"
    if [ $err = 1 ]; then
        # When patch encounters errors it exits with status 1. But we do not
        # want to count the case when there were no other problems except
        # skipping already applied chunks as an error to allow running patch
        # stages more than once. This not always works though.
        echo "$out" | awk -vRS='patching ' -vFS='\n' '!/^$/ && ( \
            $1 !~ /^file / ||
            $2 != "Reversed (or previously applied) patch detected!  Skipping patch." ||
            $3 !~ /^[[:digit:]]+ out of [[:digit:]]+ hunks? ignored$/) \
                { exit 3 }'; err=$?
    fi
    [ $err = 0 ] || exit $err
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
        pkg_run sed -E -i "s|^(prefix=)$pkg_install_prefix$|\1$pkg_install_root|" $filename
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

pkg_install_file() {
    local src="$(find_in_path "${1:?}")" dest="${2:?}"
    [ -f "$src" ] || die "failed to find '$1' in path"
    pkg_run mkdir -p "$(dirname "$dest")"
    pkg_run cp -vf "$src" "$dest"
}

pkg__fname() {
    : ${1:?}
    local name= config=
    name=${1%:*}
    if [ "$2" ] && [ "$name" != "$1" ]; then
        config=${1#*:}
    else
        config=$2
    fi
    printf '%s' "${name}${config:+:$config}"
}

pkg__export_fname() {
    printf '%s' "${jagen_include_dir:?}/$(pkg__fname "$1" "$2"):export.sh"
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
        *Makefiles) j_arg="-j$(pkg__get_jobs)" ;;
    esac
    args="$v_arg $j_arg"; args=${args# }; args=${args% }
    printf '%s' "$args"
}

pkg_get_build_profile() {
    local profile="$pkg_build_profile"
    case $profile in
        release|debug|release_with_debug)
            echo $profile ;;
        *) if [ "$pkg_build_type" = 'android-gradle' ]; then
               echo debug
           else
               echo "${jagen_build_profile:-release}"
           fi ;;
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

pkg__spawn() {
    local name="$1"
    local IFS="$jagen_S" S="$jagen_S" A=
    local layer_dirs="$(jagen__resolve_layers)" dir cmd spawn
    A="--bind-dir${S}${jagen_root_dir}${S}--bind-dir${S}${jagen_dir}"
    for dir in $layer_dirs; do
        A="$A$S--bind-dir$S$dir"
    done
    cmd=$(cat <<EOF
export jagen_recursive=1 jagen__stage_verbose=$jagen__stage_verbose
. "$jagen_root_dir/env.sh" && jagen-stage $pkg_name $pkg_stage $pkg_config
EOF
)
    if [ "$(command -v spawn-"$name")" ]; then
        spawn="spawn-$name"
    else
        spawn="${spawn_dir:?}/spawn"
    fi
    "$spawn" $A "$name" -- sh -c "$cmd"
}
