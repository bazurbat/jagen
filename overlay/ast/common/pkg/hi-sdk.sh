#!/bin/sh

jagen_pkg_unpack() {
    default_unpack

    pkg_link \
        "$jagen_src_dir/hi-kernel" \
        "$pkg_source_dir/source/kernel/linux-3.4.y"
}
