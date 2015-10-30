#!/bin/sh

use_env tools
use_toolchain target

pkg_patch() {
    pkg_run mkdir -p "$p_source_dir/sources"
    pkg_run cp "$jagen_dist_dir/dl/yamon-src-02.13.tar.gz" "$p_source_dir/sources"
}

pkg_build() {
    pkg_run make yamon-2.13
}

pkg_install() {
    pkg_run mkdir -p "$jagen_target_dir"
    pkg_run cp -f bin/zbimage-yamon-2.13 "$jagen_target_dir"
}
