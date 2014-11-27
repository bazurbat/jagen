#!/bin/sh

. "$pkg_root/lib/env.sh" ||
    { echo "Failed to load environment"; exit 1; }

: ${p_jobs:=1}
: ${p_run_on_error:=exit}

p_is_function() {
    type "$1" 2>/dev/null | grep -q 'function'
}

p_in_list() { echo "$2" | grep -qw "$1"; }

p_flags() { p_in_list "$1" "$pkg_flags"; }

p_in_path() { $(which "$1" >/dev/null 2>&1); }

p_run() {
    local cmd="$1"
    debug "$*"
    shift

    case $cmd in
        make|ninja)
            cmd="$cmd -j$p_jobs"
            ;;
    esac

    if [ "$p_log" ]; then
        $cmd "$@" >>"$p_log" 2>&1 || $p_run_on_error
    else
        $cmd "$@" || $p_run_on_error
    fi
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
        "$STRIP" -v --strip-unneeded \
            -R .comment \
            -R .GCC.command.line \
            -R .note.gnu.gold-version \
            "$f"
    done
}

p_patch() {
    p_run patch -sp1 -i "$pkg_dist_dir/patches/${1}.patch"
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
    p_run p_run sed -ri "s|libdir='/lib'|libdir='$sdk_rootfs_prefix/lib'|" $1
}

. "$pkg_lib_dir/src.sh" || exit
. "$pkg_lib_dir/stages.sh" || exit
