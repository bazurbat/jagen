#!/bin/sh

jagen_pkg_unpack() {
    pkg_unpack

    pkg_link "$pkg_source_dir" \
        "${jagen_sdk_src_dir:?}/tools"
}
