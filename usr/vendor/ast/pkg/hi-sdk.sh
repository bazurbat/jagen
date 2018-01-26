#!/bin/sh

jagen_pkg_patch() {
    pkg_run cp configs/ast2xx_hi3719cv100_cfg.mak cfg.mak
}

jagen_pkg_configure() {
    pkg_link "${kernel_dir:?}" \
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
