#!/bin/sh

jagen_pkg_unpack() {
    pkg_unpack

    pkg_link \
        "$pkg_source_dir" \
        "$jagen_sdk_dir/source/kernel/linux-3.4.y"
}
