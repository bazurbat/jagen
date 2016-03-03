#!/bin/sh

jagen_pkg_unpack() {
    local sdk_dir="$jagen_src_dir/hi-sdk"

    pkg_unpack

    if [ -d "$sdk_dir" ]; then
        pkg_link "$pkg_source_dir" "$sdk_dir/tools"
    fi
}
