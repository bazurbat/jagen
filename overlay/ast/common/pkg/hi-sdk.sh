#!/bin/sh

jagen_pkg_unpack() {
    default_unpack

    pkg_link \
        "$jagen_src_dir/hi-kernel" \
        "$pkg_source_dir/source/kernel/linux-3.4.y"

    pkg_link \
        "$jagen_src_dir/hi-sample" \
        "$pkg_source_dir/sample"
}

jagen_pkg_patch() {
    case $jagen_sdk in
        hi-linux)
            pkg_run cp configs/hi3719cdmo1b_hi3719cv100_cfg.mak cfg.mak
            ;;
    esac
}

jagen_pkg_tools_install() {
    pkg_link \
        "$jagen_build_dir/hi-sdk-tools/tools" \
        "$pkg_source_dir/tools"

    pkg_run make tools_install
}

jagen_pkg_prepare() {
    pkg_run make prepare
}

jagen_pkg_hiboot_install() {
    pkg_run make hiboot_install
}

jagen_pkg_linux_install() {
    pkg_run make linux_install
}

jagen_pkg_common_install() {
    pkg_run make common_install
}

jagen_pkg_msp_install() {
    pkg_run make msp_install
}

jagen_pkg_component_install() {
    pkg_run make component_install
}
