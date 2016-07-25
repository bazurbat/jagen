#!/bin/sh

jagen_pkg_compile() {
    pkg_run make \
        LINUX_SOURCE="$KERNEL_SRC" \
        KBUILD_OUTPUT="$KERNEL_SRC" \
        USE_KBUILD=y RUNDM=n \
        INSTALL_MOD_PATH="$pkg_install_dir"
}
