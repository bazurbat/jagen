#!/bin/sh

jagen_pkg_install() {
    local pc_names="grpc grpc++ grpc_unsecure grpc++_unsecure"
    local f filename

    pkg_install

    for f in $pc_names; do
        filename="$pkg_install_dir/lib/pkgconfig/${f}.pc"
        if [ -f "$filename" ]; then
            pkg_run chmod -x "$filename"
            pkg_run sed -ri "s|^(prefix=)(.*)$|\1$pkg_install_dir|" "$filename"
        fi
    done
}
