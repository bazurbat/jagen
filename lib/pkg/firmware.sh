#!/bin/sh

p_work_dir="$sdk_firmware_dir"
p_source_dir="${target_dir}${target_prefix}"

pkg_clean() {
    p_clean_dir "$p_work_dir"
    p_clean_dir "$p_source_dir"
}

pkg_install() {
    [ -d "$p_work_dir/bin" ] || mkdir -p "$p_work_dir/bin"

    # for bin in smplayer smmplayer csi; do
    #     p_run install -vm 755 "$p_source_dir/bin/$bin" \
    #         "$p_work_dir/bin"
    # done

    [ -d "$p_work_dir/lib" ] || mkdir -p "$p_work_dir/lib"

    echo zzzzzzzzzzzz

    echo @@ $target_dir
    echo @@ $target_prefix

    echo $p_config
    echo $p_work_dir
    echo $p_source_dir

    p_run cp -va "$p_source_dir/lib/"*.so* "$p_work_dir/lib"

    # if [ -d "$p_source_dir/lib/chicken" ]; then
    #     p_run cp -va "$p_source_dir/lib/chicken" "$p_work_dir/lib"
    # fi
}

pkg_strip() {
    p_run cd "$p_work_dir"

    p_run find lib -type f \
        "(" -name "*.la" ")" \
        -print -delete
}

pkg_deploy() {
    local out_dir="$sdk_out_dir/system"

    if [ ! -d "$out_dir" ]; then
        message "out_dir '$out_dir' is not exists, skipping deploy"
        return 0
    fi

    p_run cp -av "$p_work_dir"/* "$out_dir"
}
