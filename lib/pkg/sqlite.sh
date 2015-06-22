#!/bin/sh

pkg_patch() {
    p_autoreconf
    p_run sed -i 's/sqlite 3\.8\.4\.3/sqlite3.8.4.3/g' configure
}
