#!/bin/sh

jagen_pkg_patch() {
    pkg_run cp "configs/hi3719cdmo1b_hi3719cv100_cfg.mak" "cfg.mak"
}

jagen_pkg_build_linux() {
    pkg_run ln -rfs \
        "$jagen_src_dir/hi-kernel" \
        "source/kernel/linux-3.4.y"
    pkg_run make linux
}

jagen_pkg_install_linux() {
    pkg_run make linux_install
}

jagen_pkg_build_common() {
    pkg_run make common
}

jagen_pkg_install_common() {
    pkg_run make common_install
}

jagen_pkg_build_msp() {
    pkg_run make msp
}

jagen_pkg_install_msp() {
    pkg_run make msp_install
}

jagen_pkg_build_component() {
    pkg_run make component
}

jagen_pkg_install_component() {
    pkg_run make component_install
}
