#!/bin/sh

jagen_pkg_patch() {
    local IFS="$jagen_IFS"
    local patches_dir="$jagen_src_dir/capicxx-dbus-runtime/src/dbus-patches"
    local patches=

    # pkg_patch

    patches=$(find "$patches_dir" -maxdepth 1 -type f -name "*.patch")
    for patchfile in $patches; do
        message "applying patch $patchfile"
        pkg_run patch -p1 -i"$patchfile"
    done
}
