#!/bin/sh

psource="smp86xx_yamon_R2.13-31"

use_env tools target

pkg_prepare() {
    p_cmd mkdir -p "$psourcedir/sources"
    p_cmd cp "$distdir/dl/yamon-src-02.13.tar.gz" "$psourcedir/sources"
}

pkg_build() {
    p_make yamon-2.13
}

pkg_install() {
    p_cmd mkdir -p "$targetdir"
    p_cmd cp -f bin/zbimage-yamon-2.13 "$targetdir"
}
