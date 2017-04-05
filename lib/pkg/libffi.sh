#!/bin/sh

jagen_pkg_patch() {
    pkg_patch

    # https://bugs.gentoo.org/462814
    pkg_run sed -i -e 's:@toolexeclibdir@:$(libdir):g' Makefile.in
}

jagen_pkg_configure() {
    case $jagen_sdk in
        sigma)
            # Needed starting from GCC 4.4
            CFLAGS="$CFLAGS -mno-compact-eh"
            ;;
    esac
    pkg_configure
}
