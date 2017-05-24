#!/bin/sh

jagen_pkg_configure() {
    pkg_configure

    sed -ri \
        -e "s|^(#define PERSADMIN_ACCESSLIB_MSG_QUEUE_REQUEST \")(/PersadminAccessLibRequest\")|\1$pkg_install_dir\2|" \
        -e "s|^(#define PERSADMIN_ACCESSLIB_MSG_QUEUE_RESPONSE \")(/PersadminAccessLibResponse\")|\1$pkg_install_dir\2|" \
        -e "s|^(#define PERSADMIN_ACCESSLIB_SYNC_SEMAPHORE \")(/PersadminAccessLibSync\")|\1$pkg_install_dir\2|" \
        "$pkg_source_dir/Administrator/inc/private/ssw_pers_admin_access_lib.h"
}
