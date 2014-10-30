#!/bin/sh

psource="git git@bitbucket.org:art-system/sigma-ezboot.git"
psourcedir="$ja_srcdir/sigma-ezboot"

use_env tools target

pkg_build() {
    p_run cd "xos/xboot2/xmasboot/nand_st2"
    p_run ./build_phyblock0.bash
}

pkg_install() {
    p_run mkdir -p "$targetdir"
    p_run cd "xos/xboot2/xmasboot/nand_st2"
    p_run cp -f phyblock0-0x20000padded.AST50 "$targetdir"
    p_run cp -f phyblock0-0x20000padded.AST100 "$targetdir"
}
