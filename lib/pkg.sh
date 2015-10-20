#!/bin/sh

. "$jagen_root/lib/env.sh" ||
    { echo "Failed to load environment"; exit 1; }

: ${p_jobs:=$(nproc)}
: ${p_run_on_error:=exit}

p_run() {
    local cmd="$1"
    debug "$*"
    shift

    case $cmd in
        make)
            cmd="$cmd -j$p_jobs"
            [ "$pkg_build_verbose" = "yes" ] && cmd="$cmd V=1"
            ;;
        ninja)
            cmd="$cmd -j$p_jobs"
            [ "$pkg_build_verbose" = "yes" ] && cmd="$cmd -v"
            ;;
    esac

    $cmd "$@" || $p_run_on_error
}

p_clean_dir() {
    local dir="$1"
    if [ -d "$dir" ]; then
        rm -rf "$dir"/* ||
            die "Failed to clean directory: $dir"
    else
        mkdir -p "$dir" ||
            die "Failed to create directory: $dir"
    fi
}

p_strip() {
    local root files
    root="$1"
    files=$(find "$root" -type f -not -name "*.ko" \
        "(" -path "*/lib*" -o -path "*/bin*" -o -path "*/sbin*" ")" | \
        xargs -r file | grep "ELF.*\(executable\|shared object\).*not stripped" | cut -d: -f1)

    for f in $files; do
        p_run "$STRIP" -v --strip-unneeded \
            -R .comment \
            -R .GCC.command.line \
            -R .note.gnu.gold-version \
            "$f"
    done
}

p_patch() {
    p_run patch -p${1} -i "$pkg_patch_dir/${2}.patch"
}

p_install_modules() {
    mkdir -p "$kernel_extra_modules_dir"
    touch "$kernel_modules_dir/modules.order"
    touch "$kernel_modules_dir/modules.builtin"
    for m in "$@"; do
        local f="$PWD/${m}.ko"
        cp "$f" "$kernel_extra_modules_dir"
    done &&
        (
    cd $kernel_dir/linux && \
        /sbin/depmod -ae -F System.map -b $INSTALL_MOD_PATH $kernel_release
    )
}

p_depmod() {
    p_run /sbin/depmod -ae \
        -F "$LINUX_KERNEL/System.map" \
        -b "$INSTALL_MOD_PATH" \
        "$kernel_release"
}

p_fix_la() {
    local filename="$1"
    local prefix=${2:-"$sdk_rootfs_prefix"}
    p_run sed -i -e "s|^\(libdir=\)'\(.*\)'$|\1'${prefix}\2'|" "$filename"
}

p_autoreconf() {
    p_run autoreconf -if -I "$host_dir/share/aclocal"
}

. "$pkg_lib_dir/src.sh" || exit
. "$pkg_lib_dir/stages.sh" || exit
