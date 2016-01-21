#!/bin/sh

jagen_pkg_build() {
    # configure fails to run expat test program without this
    CFLAGS="$CFLAGS -I$pkg_install_dir/include"
    LDFLAGS="$LDFLAGS -L$pkg_install_dir/lib"

    default_build
}
