#!/bin/sh

jagen_pkg_patch() {
    pkg_link \
        "${jagen_sdk_tools_dir:?}" \
        "$pkg_source_dir/device/hisilicon/bigfish/sdk/tools"
}

jagen_pkg_compile() {
    pkg_run make BUILD_EMULATOR=false bigfish
}
