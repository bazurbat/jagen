#!/bin/sh

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
        --host="$p_system" \
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
