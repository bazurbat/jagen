#!/bin/sh

jagen_pkg_compile() {
	pkg_run make STRIP=:
}

jagen_pkg_install() {
	pkg_run install -vm755 hdparm "$pkg_install_dir/sbin"
}
