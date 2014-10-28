#!/bin/sh

pworkdir="$ja_srcdir/sigma-ezboot"

use_env tools target

pkg_build() {
    p_cmd cd "xos/xboot2/xmasboot/nand_st2"
    p_cmd ./build_phyblock0.bash
}

pkg_install() {
    p_cmd mkdir -p "$targetdir"
    p_cmd cd "xos/xboot2/xmasboot/nand_st2"
    p_cmd cp -f phyblock0-0x20000padded.AST50 "$targetdir"
    p_cmd cp -f phyblock0-0x20000padded.AST100 "$targetdir"
}
