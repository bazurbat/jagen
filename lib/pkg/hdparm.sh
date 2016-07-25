#!/bin/sh

jagen_pkg_compile() {
    pkg_run make STRIP=:
}

jagen_pkg_compile_target() {
    use_env target_toolchain
    pkg_run make STRIP=:
}

jagen_pkg_install() {
    pkg_run install -vm755 hdparm "$pkg_install_dir/sbin"
}
