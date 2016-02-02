#!/bin/sh

jagen_pkg_compile() {
    pkg_run make \
        LINUX_SOURCE="$LINUX_KERNEL" \
        KBUILD_OUTPUT="$LINUX_KERNEL" \
        USE_KBUILD=y MODINST=n RUNDM=n
}

jagen_pkg_install() {
    cd "tmp-d-kbuild" &&
    pkg_install_modules "loop"
}
