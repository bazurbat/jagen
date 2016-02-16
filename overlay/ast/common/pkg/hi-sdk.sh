#!/bin/sh

jagen_pkg_unpack() {
    pkg_unpack

    pkg_link \
        "$jagen_src_dir/hi-kernel" \
        "$pkg_source_dir/source/kernel/linux-3.4.y"

    pkg_link \
        "$jagen_src_dir/hi-sample" \
        "$pkg_source_dir/sample"
}

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
}

jagen_pkg_tools() {
    pkg_link \
        "$jagen_build_dir/hi-sdk-tools/tools" \
        "$pkg_source_dir/tools"

    pkg_run make tools_install
}

jagen_pkg_prepare() {
    pkg_run make prepare
}

jagen_pkg_hiboot() {
    pkg_run make hiboot_install
}

jagen_pkg_linux() {
    pkg_run make linux_install
}

jagen_pkg_rootfs() {
    pkg_run make rootfs_install
}

jagen_pkg_common() {
    pkg_run make common_install
}

jagen_pkg_msp() {
    pkg_run make msp_install
}

jagen_pkg_component() {
    pkg_run make component_install
}

jagen_pkg_mkload() {
    pkg_run make -C scripts load_install
}

jagen_pkg_rootbox() {
    pkg_run make rootbox_install
    pkg_run make fs
}
