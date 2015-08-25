#!/bin/sh

pkg_configure_post() {
    p_run sed -i 's/sqlite 3\.8\.4\.3/sqlite3.8.4.3/g' configure
}
