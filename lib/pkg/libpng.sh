#!/bin/sh

use_toolchain target

p_prefix="$target_prefix"
p_dest_dir="$target_dir"

pkg_build() {
	export CPPFLAGS="$CPPFLAGS $(pkg-config --cflags-only-I zlib)"
	export LDFLAGS="$LDFLAGS $(pkg-config --libs-only-L zlib)"

    p_run ./configure \
        --host="$target_system" \
		--prefix="$p_prefix"

	p_run make
}

pkg_install() {
	p_run make DESTDIR="$p_dest_dir" install
	p_fix_la "$p_dest_dir$p_prefix/lib/libpng16.la" "$p_dest_dir"
}
