#!/bin/sh

jagen_pkg_configure_target() {
    # mkheader gets confused and tries to include wrong file when target system
    # has empty vendor prefix or it differs from 'unknown' with our ARM
    # toolchains.
    if [ "$pkg_build_system" != "${pkg_build_system%gnueabi}" ]; then
        pkg_run cd "$pkg_source_dir/src/syscfg"
        pkg_run ln -snf \
            "lock-obj-pub.arm-unknown-linux-gnueabi.h" \
            "lock-obj-pub.linux-gnueabi.h" 
        pkg_run cd "$OLDPWD"
    fi

    pkg_configure
}
