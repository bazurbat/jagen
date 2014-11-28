#!/bin/sh

p_source="$pkg_dist_dir/jemalloc-3.6.0.tar.bz2"

use_toolchain target

pkg_build() {
	export je_cv_static_page_shift=12

	p_run ./configure \
		--host="$target_system" \
		--prefix="$target_prefix" \
		--disable-valgrind \
		--disable-experimental

	p_run make
}

pkg_install() {
	p_run make DESTDIR="$target_dir" install
}
