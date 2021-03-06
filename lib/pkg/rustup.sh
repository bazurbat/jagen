#!/bin/sh

jagen_stage_install() {
    local name
    pkg_run cd "${CARGO_HOME:?}/bin"
    pkg_run mkdir -p "$pkg_install_dir/bin"
    for name in *; do
        pkg_run ln -fs "$CARGO_HOME/bin/$name" "$pkg_install_dir/bin/$name"
    done
}
