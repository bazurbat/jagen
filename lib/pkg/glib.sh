#!/bin/sh

jagen_pkg_patch() {
    default_patch

    # leave python shebang alone
    pkg_run sed -ie '/${PYTHON}/d' \
        glib/Makefile.am glib/Makefile.in
}

jagen_pkg_build() {
    local cache="cross_cache.conf"
    echo "glib_cv_stack_grows=no" > "$cache"
    echo "ac_cv_func_posix_getpwuid_r=yes" >> "$cache"
    echo "ac_cv_func_posix_getgrgid_r=yes" >> "$cache"
    echo "glib_cv_uscore=yes" >> "$cache"

    pkg_run "$pkg_source_dir/configure" \
        --cache-file="$cache" \
        --host="$pkg_system" \
        --prefix="$pkg_prefix" \
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

    pkg_run make
}
