#!/bin/sh

if [ -z "$jagen_toolchain_prefix" ]; then
	jagen_toolchain_prefix="${jagen_bin_dir:?}/${jagen_target_system:?}-"
fi

: ${jagen_target_dir:?}

export PKG_CONFIG_SYSROOT_DIR="$jagen_target_dir"
export PKG_CONFIG_LIBDIR="$jagen_target_dir/lib/pkgconfig"
export PKG_CONFIG_PATH="$jagen_target_dir/usr/lib/pkgconfig"

# pkg-config tries to be smart and removes -I and -L flags from it's output
# when they resemble system paths. This causes SYSROOT_DIR to not be added to
# them, which prevents packages to find each other when building the sysroot
# itself.
export PKG_CONFIG_ALLOW_SYSTEM_CFLAGS=1
export PKG_CONFIG_ALLOW_SYSTEM_LIBS=1

export ARCH="${jagen_target_arch:?}"
export CROSS_COMPILE="${jagen_toolchain_prefix:?}"
