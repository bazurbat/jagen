#!/bin/sh

jagen_pkg_configure() {
    # The PCL build assumes that persistence common object headers are
    # installed in the system include path (/usr/include or such) and
    # additionally hardcodes CFLAGS in the configure script. We append -I
    # option to the CFLAGS to support installing in the custom prefix.

    local dir="$(eval echo $pkg_install_dir/include)"
    pkg_run sed -ri "s|^CFLAGS=\"(.*)\"$|CFLAGS=\"\1 -g -I'$dir'\"|" \
        "$pkg_source_dir/configure"

    pkg_configure
}

jagen_pkg_install() {
    pkg_install

    pkg_run cp -f "$pkg_source_dir/config/pclCustomLibConfigFile.cfg" \
        "$pkg_install_dir/etc/pclCustomLibConfigFileDefault.cfg"

    pkg_run cat >"$pkg_install_dir/etc/pclCustomLibConfigFile.cfg" <<EOF
default $pkg_install_dir/lib/libpers_common.so.0 init sync
EOF
}
