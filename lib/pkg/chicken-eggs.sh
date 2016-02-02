#!/bin/sh

delete_install_targets() {
    pkg_run find "$pkg_build_dir" -name "*-install" -delete
}

jagen_pkg_configure_host() {
    delete_install_targets

    pkg_configure
}

jagen_pkg_configure_target() {
    delete_install_targets

    pkg_configure $(pkg_using_host_chicken)
}

jagen_pkg_install() {
    :
}
