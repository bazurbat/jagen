#!/bin/sh

delete_install_targets() {
    pkg_run find "$pkg_build_dir" -name "*-install" -delete
}

jagen_pkg_build_host() {
    delete_install_targets

    default_build
}

jagen_pkg_build_target() {
    delete_install_targets

    default_build $(pkg_cmake_use_host_chicken)
}

jagen_pkg_install() {
    :
}
