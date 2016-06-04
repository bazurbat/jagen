#!/bin/sh

jagen_pkg_install() {
    local dest="${pkg_install_dir:?}/bin"
    set -- ${pkg_source:?}

    pkg_run mkdir -p "$dest"
    if [ -f "${jagen_dist_dir:?}/repo" ]; then
        pkg_run cp -v "$jagen_dist_dir/repo" "$dest"
    else
        curl "$2" > "$dest/repo" || die
    fi
    pkg_run chmod +x "$dest/repo"
}
