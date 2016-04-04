#!/bin/sh

jagen_pkg_configure() {
    pkg_configure \
        --with-lib="$pkg_install_dir/lib/skalibs"

    # Fix duplicated CROSS_COMPILE prefix
    pkg_run sed -ri "s|^(CC := )$CROSS_COMPILE(.*)$|\1\2|" config.mak
}
