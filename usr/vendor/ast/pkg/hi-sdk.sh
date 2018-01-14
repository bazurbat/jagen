#!/bin/sh

# Reset user-supplied build flags. This SDK assumes a specific environment and
# passing arbitrary options can break it in various ways.
export CFLAGS=''
export CXXFLAGS=''
export ASMFLAGS=''
export LDFLAGS=''

jagen_pkg_patch() {
    pkg_run cp configs/ast2xx_hi3719cv100_cfg.mak cfg.mak
}

jagen_pkg_configure() {
    pkg_link "${jagen_kernel_dir:?}" \
        "$pkg_source_dir/source/kernel/linux-3.4.y"

    pkg_run make msp_prepare
}

jagen_pkg_compile() {
    pkg_run make hiboot_install
    pkg_run make common_install
    pkg_run make msp_install
    pkg_run make msp_mod_install
    pkg_run make component_install
    pkg_run make -C scripts load_install
}
