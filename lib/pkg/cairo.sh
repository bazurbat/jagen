#!/bin/sh

jagen_pkg_configure() {
    # Configure fails to find zlib without this
    pkg_configure \
        CFLAGS="-I$pkg_install_dir/include" \
        LDFLAGS="-L$pkg_install_dir/lib"
}
