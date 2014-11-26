#!/bin/sh

p_source="$pkg_dist_dir/glib-2.40.2.tar.xz"

use_env target

pkg_patch() {
    # removes hard dependency on python
    p_run patch -p1 \
        < "$pkg_dist_dir"/patches/glib-2.40.0-external-gdbus-codegen.patch
    # leave python shebang alone
    p_run sed -e '/${PYTHON}/d' \
        -i glib/Makefile.{am,in}
    p_run autoreconf -vif
}

pkg_build() {
    local cache="cross_cache.conf"
    echo "glib_cv_stack_grows=no" > "$cache"
    echo "ac_cv_func_posix_getpwuid_r=yes" >> "$cache"
    echo "ac_cv_func_posix_getgrgid_r=yes" >> "$cache"
    echo "glib_cv_uscore=yes" >> "$cache"

    export LIBFFI_CFLAGS="-I$(echo ${target_dir}${target_prefix}/lib/libffi-*/include)"
    export LIBFFI_LIBS="-L${target_dir}${target_prefix}/lib -lffi"

    p_run ./configure \
        --cache-file="$cache" \
        --host="$target_system" \
        --prefix="$target_prefix" \
        --disable-gc-friendly \
        --disable-mem-pools \
        --disable-rebuilds \
        --disable-installed-tests \
        --disable-always-build-tests \
        --enable-largefile \
        --disable-static \
        --enable-shared \
        --enable-fast-install \
        --enable-libtool-lock \
        --disable-selinux \
        --disable-fam \
        --disable-xattr \
        --disable-libelf \
        --disable-gtk-doc \
        --disable-gtk-doc-html \
        --disable-gtk-doc-pdf \
        --disable-man \
        --disable-dtrace \
        --disable-systemtap \
        --disable-coverage \
        --disable-Bsymbolic \
        --disable-znodelete

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$target_dir" install
}
