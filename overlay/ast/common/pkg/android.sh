#!/bin/sh

jagen_pkg_unpack() {
    default_unpack

    pkg_link \
        "$jagen_src_dir/hi-sdk" \
        "$pkg_source_dir/device/hisilicon/bigfish/sdk"
}

jagen_pkg_build() {
    pkg_link "$pkg_build_dir" "$pkg_source_dir/out"

    use_env lunch || return

    pkg_run make bigfish_emmc
}

jagen_pkg_install() {
    :
}
