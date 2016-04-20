#!/bin/sh

jagen_pkg_patch() {
    if [ "$jagen_sdk" = "hi-linux" ]; then
        case $jagen_target_board in
            ast2*)
                pkg_run cp configs/ast2xx_hi3719cv100_cfg.mak cfg.mak
                ;;
            *)
                pkg_run cp configs/hi3719cdmo1b_hi3719cv100_cfg.mak cfg.mak
                ;;
        esac
    fi

    pkg_link "${jagen_kernel_src_dir:?}" \
        "$pkg_source_dir/source/kernel/linux-3.4.y"
}

jagen_pkg_prepare() {
    pkg_run make prepare
}

jagen_pkg_hiboot() {
    pkg_run make hiboot_install
}

jagen_pkg_linux() {
    pkg_run make linux_install
    pkg_run make -C "$jagen_kernel_src_dir" \
        INSTALL_MOD_PATH="$pkg_install_dir" \
        modules_install
}

jagen_pkg_common() {
    pkg_run make common_install
}

jagen_pkg_msp() {
    pkg_run make msp_install
    pkg_run make msp_mod_install
}

jagen_pkg_component() {
    pkg_run make component_install
}

jagen_pkg_mkload() {
    pkg_run make -C scripts load_install
}
