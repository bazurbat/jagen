#!/bin/sh

jagen_pkg_patch() {
    pkg_patch

    enable_option() {
        sed -i -e "/#define $1/a #define $1" \
            include/freetype/config/ftoption.h \
            || die "unable to enable option $1"
    }

    disable_option() {
        sed -i -e "/#define $1/ { s:^:/*:; s:$:*/: }" \
            include/freetype/config/ftoption.h \
            || die "unable to disable option $1"
    }

    enable_option FT_CONFIG_OPTION_SUBPIXEL_RENDERING
    disable_option TT_CONFIG_OPTION_BYTECODE_INTERPRETER
    enable_option TT_CONFIG_OPTION_UNPATENTED_HINTING
}
