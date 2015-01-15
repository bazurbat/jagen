#!/bin/sh

p_source="$pkg_dist_dir/freetype-2.5.0.1.tar.bz2"

use_toolchain target

p_prefix="$target_prefix"
p_dest_dir="$target_dir"

pkg_patch() {
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

pkg_build() {
    p_run ./configure \
        --host="$target_system" \
        --prefix="$p_prefix" \
        --disable-static \
        --without-bzip2 \
        --without-png \
        --without-old-mac-fonts \
        --without-fsspec \
        --without-fsref \
        --without-quickdraw-toolbox \
        --without-quickdraw-carbon \
        --without-ats

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$p_dest_dir" install
    p_fix_la "$p_dest_dir$p_prefix/lib/libfreetype.la" "$p_dest_dir"
}
