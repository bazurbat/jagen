#!/bin/sh

jagen_pkg_compile() {
    local programs="csi smplayer"

    mkdir -p bin lib

    for name in $programs; do
        if [ -x "$pkg_source_dir/bin/$name" ]; then
            pkg_run install -m755 "$pkg_source_dir/bin/$name" "bin"
        else
            warning "$pkg_source_dir/bin/$name not found"
        fi
    done

    if [ -d "$pkg_source_dir/lib" ]; then
        pkg_run cp -a "$pkg_source_dir/lib/"*.so* "lib"
        pkg_run chmod 755 lib/*.so*
    fi

    if [ -d "$pkg_source_dir/lib/chicken" ]; then
        pkg_run cp -a "$pkg_source_dir/lib/chicken" "$pkg_work_dir/lib"
    fi
}

jagen_pkg_install() {
    local out_dir="$jagen_sdk_staging_dir"

    if ! [ -d "$out_dir" ]; then
        message "output directory $out_dir is not exists, skipping deployment"
        return 0
    fi

    pkg_run cp -a "$pkg_build_dir"/* "$out_dir"
}
