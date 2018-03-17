#!/bin/sh

jagen_pkg_install() {
    local name
    pkg_run cd "$pkg_build_dir/bin"
    pkg_run mkdir -p "$pkg_install_dir/bin"
    for name in *; do
        pkg_run ln -fs "$pkg_build_dir/bin/$name" "$pkg_install_dir/bin/$name"
    done
}
