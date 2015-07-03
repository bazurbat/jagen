#!/bin/sh

p_source_dir="$pkg_src_dir/sigma-ezboot"
p_source_branch="sdk4"

use_env tools
use_toolchain target

export RMCFLAGS="$RMCFLAGS \
-DRMCHIP_ID=RMCHIP_ID_SMP8652 \
-DRMCHIP_REVISION=3 \
-DWITH_PROD=1"

pkg_build() {
    PATH="$SMP86XX_TOOLCHAIN_PATH/bin:$PATH"

    p_run cd "xos/xboot2/xmasboot/nand_st2"
    p_run ./build_phyblock0.bash
}

pkg_install() {
    p_run mkdir -p "$target_dir"
    p_run cd "xos/xboot2/xmasboot/nand_st2"
    p_run cp -f phyblock0-0x20000padded.AST50 "$target_dir"
    p_run cp -f phyblock0-0x20000padded.AST100 "$target_dir"
}
