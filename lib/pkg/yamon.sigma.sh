#!/bin/sh

p_source="$pkg_dist_dir/smp86xx_yamon_R2.13-31.tar.bz2"

use_env tools
use_toolchain target

pkg_patch() {
    p_run mkdir -p "$p_source_dir/sources"
    p_run cp "$pkg_dist_dir/dl/yamon-src-02.13.tar.gz" "$p_source_dir/sources"
}

pkg_build() {
    p_run make yamon-2.13
}

pkg_install() {
    p_run mkdir -p "$target_dir"
    p_run cp -f bin/zbimage-yamon-2.13 "$target_dir"
}
