#!/bin/sh

use_env tools

export RMCFLAGS="$RMCFLAGS \
-DRMCHIP_ID=RMCHIP_ID_SMP8652 \
-DRMCHIP_REVISION=3 \
-DWITH_PROD=1"

jagen_pkg_build() {
    add_PATH "$SMP86XX_TOOLCHAIN_PATH/bin"
    add_PATH "$pkg_dest_dir/bin"

    pkg_run cd "xos/xboot2/xmasboot/nand_st2"
    pkg_run ./build_phyblock0.bash
}

jagen_pkg_install() {
    pkg_run mkdir -p "$jagen_target_dir"
    pkg_run cd "xos/xboot2/xmasboot/nand_st2"
    pkg_run cp -f phyblock0-0x20000padded.AST50 "$jagen_target_dir"
    pkg_run cp -f phyblock0-0x20000padded.AST100 "$jagen_target_dir"
}
