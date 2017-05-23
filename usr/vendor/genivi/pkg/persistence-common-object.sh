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
    local root_path="${pkg_install_dir:?}/Data"

    pkg_install

    pkg_run mkdir -p "$root_path/mnt-c"
    pkg_link "$root_path/mnt-c" "$root_path/mnt-wt"
}
