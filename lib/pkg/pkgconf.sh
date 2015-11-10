#!/bin/sh

jagen_pkg_install() {
    default_install
    pkg_run cd "$pkg_dest_dir/$pkg_prefix/bin"
    pkg_run ln -sf pkgconf pkg-config
}
