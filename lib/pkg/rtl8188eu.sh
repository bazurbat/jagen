#!/bin/sh

jagen_pkg_compile() {
    pkg_compile KSRC="$KERNEL_SRC"
}

jagen_pkg_install() {
    pkg_run make -C "$KERNEL_SRC" M="$PWD" INSTALL_MOD_PATH="$pkg_install_dir" \
        modules_install
}
