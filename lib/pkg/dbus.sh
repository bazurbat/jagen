#!/bin/sh

jagen_pkg_build() {
    # configure fails to run expat test program without this
    export CFLAGS="$CFLAGS -I$pkg_install_dir/include"
    export LDFLAGS="$LDFLAGS -L$pkg_install_dir/lib"

    default_build
}
