#!/bin/sh

jagen_pkg_configure() {
    export CPPFLAGS="$CPPFLAGS $(pkg-config --cflags-only-I zlib)"
    export LDFLAGS="$LDFLAGS $(pkg-config --libs-only-L zlib)"

    pkg_configure
}
