#!/bin/sh

use_env tools
use_toolchain target

jagen_pkg_patch() {
    pkg_run mkdir -p "$pkg_source_dir/sources"
    pkg_run cp "$jagen_dist_dir/dl/yamon-src-02.13.tar.gz" "$pkg_source_dir/sources"
}

jagen_pkg_build() {
    pkg_run make yamon-2.13
}

jagen_pkg_install() {
    pkg_run mkdir -p "$jagen_target_dir"
    pkg_run cp -f bin/zbimage-yamon-2.13 "$jagen_target_dir"
}
