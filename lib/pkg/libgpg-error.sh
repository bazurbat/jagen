#!/bin/sh

jagen_pkg_install() {
    pkg_install || return

    if [ "$pkg_sysroot" ]; then
        pkg_run sed -ri "s|^(prefix=)$|\1$pkg_sysroot|" \
            "${pkg_install_dir:?}/bin/gpg-error-config"
    fi
}
