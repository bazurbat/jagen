#!/bin/sh

pkg_source_dir="$pkg_install_dir"

jagen_pkg_compile() {
    local filter_file="$(find_file filter.txt)"

    pkg_run rsync -va --delete --delete-excluded \
        --filter=". ${filter_file}" \
        "$pkg_source_dir" \
        "$pkg_build_dir/"
}
