#!/bin/sh

jagen_pkg_install() {
    # Some packages include "dlt.h", some include "dlt/dlt.h", this is a
    # workaround to avoid patching everything.
    pkg_run sed -i \
        -e 's|^Cflags: -I${includedir}/dlt|Cflags: -I${includedir} -I${includedir}/dlt|' \
        "$pkg_build_dir/automotive-dlt.pc"

    pkg_install
}
