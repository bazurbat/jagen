#!/bin/sh

jagen_pkg_build() {
    local out_dir="$pkg_source_dir/out"

    pkg_run rm -rf "$out_dir"
    pkg_run ln -sr "$pkg_build_dir" "$out_dir"

    use_env lunch || return

    pkg_run make bigfish_emmc
}
