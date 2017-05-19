#!/bin/sh

jagen_pkg_autoreconf() {
    pkg_run mkdir -p "$pkg_source_dir/m4"
    pkg_autoreconf
}
