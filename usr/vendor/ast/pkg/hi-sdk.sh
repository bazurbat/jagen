#!/bin/sh

jagen_pkg_patch() {
    if [ "$jagen_sdk" = "hi-linux" ]; then
        case ${jagen_target_board:?} in
            ast2*)
                pkg_run cp configs/ast2xx_hi3719cv100_cfg.mak cfg.mak
                ;;
            *)
                pkg_run cp configs/hi3719cdmo1b_hi3719cv100_cfg.mak cfg.mak
                ;;
        esac
    fi
}

jagen_pkg_configure() {
    pkg_link "${jagen_kernel_dir:?}" \
        "$pkg_source_dir/source/kernel/linux-3.4.y"

    pkg_link "${jagen_build_dir:?}/hi-sdk-tools/tools" \
        "$pkg_source_dir/tools"

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
