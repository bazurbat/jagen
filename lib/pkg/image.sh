#!/bin/sh

pkg_source_dir="$pkg_install_dir"

jagen_pkg_compile() {
    pkg_sync_dirs "$pkg_source_dir" "$pkg_build_dir" "image_contents.txt"
}
