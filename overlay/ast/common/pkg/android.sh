#!/bin/sh

jagen_pkg_patch() {
    pkg_link \
        "$jagen_src_dir/hi-sdk" \
        "$pkg_source_dir/device/hisilicon/bigfish/sdk"
}

jagen_pkg_compile() {
    pkg_run make BUILD_EMULATOR=false bigfish_emmc
}
