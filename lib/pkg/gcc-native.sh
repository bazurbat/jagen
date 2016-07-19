#!/bin/sh

jagen_pkg_install() {
    local name src_path dest_path
    : ${pkg_source_dir:?}
    : ${jagen_bin_dir:?}

    for name in ${toolchain_programs:?}; do
        src_path="$pkg_source_dir/bin/$name"
        dest_path="$jagen_bin_dir/$name"
        if [ -x "$src_path" ]; then
            toolchain_generate_wrapper "$dest_path" "$src_path" || return
            chmod +x "$dest_path" || return
        fi
    done
}
