#!/bin/sh

jagen_pkg_unpack() {
    default_unpack

    pkg_link \
        "$jagen_src_dir/hi-sdk" \
        "$pkg_source_dir/device/hisilicon/bigfish/sdk"
}

jagen_pkg_build() {
    use_env lunch || return

    pkg_run make BUILD_EMULATOR=false bigfish_emmc
}

jagen_pkg_install() {
    :
}
