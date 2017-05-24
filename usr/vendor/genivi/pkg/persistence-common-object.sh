#!/bin/sh

jagen_pkg_autoreconf() {
    # LIBPERSOCOMMON_VERSION_S is not defined anywhere
    sed -ri \
        -e 's|LIBPERSOCOMMON_VERSION_S|PERSCOMMON_PACKAGE_VERSION_S|g' \
        "$pkg_source_dir/configure.ac"

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
