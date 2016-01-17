#!/bin/sh

jagen_pkg_unpack() {
    local dest="$jagen_host_dir/bin"
    set -- $pkg_source

    pkg_run mkdir -p "$dest"
    if [ -f "$jagen_dist_dir/repo" ]; then
        pkg_run cp -v "$jagen_dist_dir/repo" "$dest/repo"
    else
        pkg_run curl "$2" > "$dest/repo"
    fi
    pkg_run chmod +x "$dest/repo"
}
