#!/bin/sh

use_toolchain target

jagen_pkg_build() {
	export je_cv_static_page_shift=12

	pkg_run ./configure \
		--host="$target_system" \
		--prefix="$jagen_target_prefix" \
		--disable-valgrind \
		--disable-experimental

	pkg_run make
}

jagen_pkg_install() {
	pkg_run make DESTDIR="$jagen_target_dir" install
}
