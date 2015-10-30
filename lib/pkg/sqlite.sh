#!/bin/sh

pkg_configure_post() {
    pkg_run sed -i 's/sqlite 3\.8\.4\.3/sqlite3.8.4.3/g' configure
}
