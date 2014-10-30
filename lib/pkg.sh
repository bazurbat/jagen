#!/bin/sh

p_is_function() {
    type "$1" 2>/dev/null | grep -q 'function'
}

p_run() {
    debug "$*"
    "$@" >>"$p_log" 2>&1 || exit
}

p_clean() {
    if [ -d "$1" ]; then
        p_run rm -fr "$1"
    fi
    p_run mkdir -p "$1"
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
    p_run patch -sp1 -i "$p_dist_dir/patches/${1}.patch"
}

p_install_modules() {
    mkdir -p "$kernelextramodulesdir"
    touch "$kernelmodulesdir/modules.order"
    touch "$kernelmodulesdir/modules.builtin"
    for m in "$@"; do
        local f="$PWD/${m}.ko"
        cp "$f" "$kernelextramodulesdir"
    done &&
        (
    cd $kerneldir/linux && \
        /sbin/depmod -ae -F System.map -b $INSTALL_MOD_PATH $kernelrelease
    )
}

p_depmod() {
    /sbin/depmod -ae \
        -F "$LINUX_KERNEL/System.map" \
        -b "$INSTALL_MOD_PATH" \
        "$kernelrelease"
}

p_fix_la() {
    p_run p_run sed -ri "s|libdir='/lib'|libdir='$sdk_rootfs_prefix/lib'|" $1
}

pkg_clean() {
    p_run rm -rf "$p_work_dir"/*
}

. "$ja_lib_dir/unpack.sh" || exit
