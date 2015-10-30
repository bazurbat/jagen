#!/bin/sh

use_toolchain target

pkg_build() {
	export je_cv_static_page_shift=12

	p_run ./configure \
		--host="$target_system" \
		--prefix="$jagen_target_prefix" \
		--disable-valgrind \
		--disable-experimental

	p_run make
}

pkg_install() {
	p_run make DESTDIR="$jagen_target_dir" install
}
