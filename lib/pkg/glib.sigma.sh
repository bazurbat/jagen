#!/bin/sh

use_toolchain target

p_prefix="$target_prefix"
p_dest_dir="$target_dir"

pkg_patch() {
    # leave python shebang alone
    p_run sed -ie '/${PYTHON}/d' \
        glib/Makefile.am glib/Makefile.in
}

pkg_build() {
    local cache="cross_cache.conf"
    echo "glib_cv_stack_grows=no" > "$cache"
    echo "ac_cv_func_posix_getpwuid_r=yes" >> "$cache"
    echo "ac_cv_func_posix_getgrgid_r=yes" >> "$cache"
    echo "glib_cv_uscore=yes" >> "$cache"

    p_run ./configure \
        --cache-file="$cache" \
        --host="$target_system" \
        --prefix="$p_prefix" \
        --disable-mem-pools \
        --disable-rebuilds \
        --disable-selinux \
        --disable-fam \
        --disable-xattr \
        --disable-libelf \
        --disable-gtk-doc-html \
        --disable-man \
        --disable-dtrace \
        --disable-systemtap \
        --disable-coverage \
        --disable-Bsymbolic \
        --disable-znodelete

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$p_dest_dir" install
    for f in glib-2.0 gthread-2.0 gobject-2.0 gmodule-2.0 gio-2.0; do
        p_fix_la "$p_dest_dir$p_prefix/lib/lib${f}.la" "$p_dest_dir"
    done
}
