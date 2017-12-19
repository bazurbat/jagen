#!/bin/sh

require toolchain

jagen_pkg_install() {
    toolchain_generate_wrappers   \
        "${jagen_bin_dir:?}"      \
        "${pkg_source_dir:?}/bin"
}
