#!/bin/sh

psource="$pkg_distdir/smp86xx_yamon_R2.13-31.tar.bz2"

use_env tools target

pkg_prepare() {
    p_run mkdir -p "$psourcedir/sources"
    p_run cp "$pkg_distdir/dl/yamon-src-02.13.tar.gz" "$psourcedir/sources"
}

pkg_build() {
    p_run make yamon-2.13
}

pkg_install() {
    p_run mkdir -p "$targetdir"
    p_run cp -f bin/zbimage-yamon-2.13 "$targetdir"
}
