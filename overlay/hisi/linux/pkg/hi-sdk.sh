#!/bin/sh

jagen_pkg_patch() {
    pkg_run tar -xf "$jagen_dist_dir/hi-tools.tar"
    pkg_run cp "configs/hi3719cdmo1b_hi3719cv100_cfg.mak" "cfg.mak"
}

jagen_pkg_tools() {
    pkg_run make tools_install
}

jagen_pkg_prepare() {
    pkg_run ln -rfs \
        "$jagen_src_dir/hi-kernel" \
        "source/kernel/linux-3.4.y"
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
