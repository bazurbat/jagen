#!/bin/sh

p_work_dir="$jagen_install_dir"
p_source_dir="${jagen_target_dir}${jagen_target_prefix}"

pkg_clean() {
    p_clean_dir "$p_work_dir"
    p_clean_dir "$p_source_dir"
}

pkg_install() {
    local bins="csi smplayer"

    [ -d "$p_work_dir/bin" ] || mkdir -p "$p_work_dir/bin"
    [ -d "$p_work_dir/lib" ] || mkdir -p "$p_work_dir/lib"

    for bin in $bins; do
        pkg_run install -m755 "$p_source_dir/bin/$bin" "$jagen_install_dir/bin"
    done

    pkg_run cp -va "$p_source_dir/lib/"*.so* "$p_work_dir/lib"

    if [ -d "$p_source_dir/lib/chicken" ]; then
        pkg_run cp -va "$p_source_dir/lib/chicken" "$p_work_dir/lib"
    fi
}

pkg_strip() {
    pkg_run cd "$p_work_dir"

    pkg_run find lib -type f \
        "(" -name "*.la" ")" \
        -print -delete
}

pkg_deploy() {
    : ${sdk_out_dir:?}
    local out_dir="$sdk_out_dir/system"

    if [ ! -d "$out_dir" ]; then
        message "out_dir '$out_dir' is not exists, skipping deploy"
        return 0
    fi

    pkg_run cp -av "$p_work_dir"/* "$out_dir"
}
