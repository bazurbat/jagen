#!/bin/sh

pkg_work_dir="$jagen_install_dir"
pkg_source_dir="${jagen_target_dir}${jagen_target_prefix}"

jagen_pkg_clean() {
    pkg_clean_dir "$pkg_work_dir"
    pkg_clean_dir "$pkg_source_dir"
}

jagen_pkg_install() {
    local bins="csi smplayer"

    [ -d "$pkg_work_dir/bin" ] || mkdir -p "$pkg_work_dir/bin"
    [ -d "$pkg_work_dir/lib" ] || mkdir -p "$pkg_work_dir/lib"

    for bin in $bins; do
        if [ -x "$pkg_source_dir/bin/$bin" ]; then
            pkg_run install -m755 "$pkg_source_dir/bin/$bin" "$jagen_install_dir/bin"
        else
            warning "$pkg_source_dir/bin/$bin not found"
        fi
    done

    if [ -d "$pkg_source_dir/lib" ]; then
        pkg_run cp -va "$pkg_source_dir/lib/"*.so* "$pkg_work_dir/lib"
    fi

    if [ -d "$pkg_source_dir/lib/chicken" ]; then
        pkg_run cp -va "$pkg_source_dir/lib/chicken" "$pkg_work_dir/lib"
    fi
}

jagen_pkg_strip() {
    pkg_run cd "$pkg_work_dir"

    pkg_run find lib -type f \
        "(" -name "*.la" ")" \
        -print -delete
}

jagen_pkg_deploy() {
    : ${jagen_android_out_dir:?}
    local out_dir="$jagen_android_out_dir/system"

    if [ ! -d "$out_dir" ]; then
        message "out_dir '$out_dir' is not exists, skipping deploy"
        return 0
    fi

    pkg_run cp -av "$pkg_work_dir"/* "$out_dir"
}
