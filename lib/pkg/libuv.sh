#!/bin/sh

pkg_patch() {
    p_run cd "$p_source_dir"
    p_run ./autogen.sh
}

pkg_build_host() {
    p_run "$p_source_dir/configure" \
        --prefix="$host_prefix"

    p_run make
}

pkg_install_host() {
    p_run make DESTDIR="$host_dir" install
    p_fix_la "${host_dir}${host_prefix}/lib/libuv.la" "$host_dir"
}

pkg_build_target() {
    p_run "$p_source_dir/configure" \
        --host="$target_system" \
        --prefix="$target_prefix"

    p_run make
}

pkg_install_target() {
    p_run make DESTDIR="$target_dir" install
    p_fix_la "${target_dir}${target_prefix}/lib/libuv.la" "$target_dir"
}
