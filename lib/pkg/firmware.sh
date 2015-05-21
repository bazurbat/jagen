#!/bin/sh

use_toolchain target

p_work_dir="$sdk_firmware_dir"
p_source_dir="${target_dir}${target_prefix}"

pkg_clean() {
    p_clean_dir "$p_work_dir"
    p_clean_dir "$p_source_dir"
}

pkg_unpack() {
    p_run cd "$p_work_dir"

    p_run install -d -m 755 bin dev etc home lib libexec mnt proc run sbin share sys usr var
    p_run install -d -m 700 root
    p_run install -d -m 1777 tmp
}

pkg_strip() {
    p_run cd "$p_work_dir"

    p_run find lib -type f \
        "(" -name "*.a" -o -name "*.la" ")" \
        -print -delete

    _jagen src status > "$p_work_dir/heads" || die
}
