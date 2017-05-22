#!/bin/sh

jagen_pkg_autoreconf() {
    pkg_run mkdir -p "$pkg_source_dir/m4"
    pkg_autoreconf
}

jagen_pkg_configure() {
    pkg_run sed -ri \
        -e "s|^(#define PERS_ORG_ROOT_PATH) \"(.*)\"$|\1 \"$pkg_install_dir/Data\"|" \
        "$pkg_source_dir/inc/protected/persComDataOrg.h"
    pkg_configure
}

jagen_pkg_install() {
    pkg_install

    pkg_run mkdir -p "$pkg_install_dir/mnt-c"
    ( cd "$pkg_install_dir" && ln -sT "mnt-c" "mnt-wt")
}
