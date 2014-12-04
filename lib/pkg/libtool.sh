#!/bin/sh

use_env tools

p_prefix="$tools_prefix"
p_dest_dir="$tools_dir"

pkg_patch() {
	export LIBTOOLIZE=echo

	p_run patch -p1 \
		< "$pkg_patch_dir/libtool-2.4.3-no-clean-gnulib.patch"
	p_run patch -p1 \
		< "$pkg_patch_dir/libtool-2.4.3-test-cmdline_wrap.patch"
	pushd libltdl >/dev/null
	p_run autoreconf -if
	popd >/dev/null
	p_run autoreconf -if
}

pkg_build() {
	export CONFIG_SHELL=/bin/bash

    p_run ./configure \
        --prefix="$p_prefix" \
		--disable-ltdl-install

	p_run make
}

pkg_install() {
    p_run make DESTDIR="$p_dest_dir" install
}
