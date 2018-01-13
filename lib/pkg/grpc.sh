#!/bin/sh

jagen_pkg_configure() {
    local filter_file="$(find_in_path src_filter.txt)"
    pkg_run rsync -va \
        ${filter_file:+--filter="merge $filter_file"} \
        "$pkg_source_dir"/ "$pkg_build_dir"
}

jagen_pkg_compile_target() {
    export PKG_CONFIG_SYSROOT_DIR="$jagen_target_dir"
    export GRPC_CROSS_COMPILE='true'
    export GRPC_CROSS_AROPTS='r'
    # export HAS_PKG_CONFIG='false'
    # export PROTOBUF_CONFIG_OPTS="--host='$pkg_build_system' --with-protoc='$jagen_host_dir/bin/protoc'"
    pkg_compile HOST_CC=gcc HOST_CXX=g++ HOST_LD=gcc HOST_LDXX=g++
}

jagen_pkg_install_host() {
    local pc_names="grpc grpc++ grpc_unsecure grpc++_unsecure"
    local f filename

    pkg_install
    pkg_run_ldconfig

    for f in $pc_names; do
        filename="$pkg_install_dir/lib/pkgconfig/${f}.pc"
        if [ -f "$filename" ]; then
            pkg_run chmod -x "$filename"
            pkg_run sed -ri "s|^(prefix=)(.*)$|\1$pkg_install_prefix|" "$filename"
        fi
    done
}
