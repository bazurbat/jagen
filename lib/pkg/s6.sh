#!/bin/sh

jagen_pkg_configure() {
    pkg_configure \
        --with-lib="$pkg_install_dir/lib/execline" \
        --with-lib="$pkg_install_dir/lib/skalibs"
}
