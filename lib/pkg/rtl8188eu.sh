#!/bin/sh

jagen_pkg_compile() {
    pkg_compile KSRC="$KDIR"
}

jagen_pkg_install() {
    pkg_run make -C "$KDIR" M="$PWD" INSTALL_MOD_PATH="$pkg_install_dir" \
        modules_install
}
