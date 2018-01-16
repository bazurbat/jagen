#!/bin/sh

jagen_pkg_install() {
    pkg_run install -vm755 hdparm "$pkg_install_dir/sbin"
}
