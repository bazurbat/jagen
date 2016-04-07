#!/bin/sh

jagen_pkg_autoreconf() {
    pkg_run cd "$pkg_source_dir"
    pkg_run ./bootstrap
}
