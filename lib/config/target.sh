#!/bin/sh

pkg_install_prefix=/
pkg_install_root=$jagen_target_dir

if [ "${jagen_target_cmake_module_path+!}" ]; then
    jagen_cmake_module_path=$jagen_target_cmake_module_path
fi

pkg_build_cmake_toolchain_file=$jagen_cmake_toolchain_file

export CFLAGS="-O2"
export CXXFLAGS="$CFLAGS"
export LDFLAGS=""

export PKG_CONFIG_SYSROOT_DIR="$pkg_install_root"
export PKG_CONFIG_LIBDIR="$pkg_install_root/lib/pkgconfig"
export PKG_CONFIG_PATH="$pkg_install_root/usr/lib/pkgconfig"

# pkg-config tries to be smart and removes -I and -L flags from it's output
# when they resemble system paths. This causes SYSROOT_DIR to not be added to
# them, which prevents packages to find each other when building the sysroot
# itself.
export PKG_CONFIG_ALLOW_SYSTEM_CFLAGS=1
export PKG_CONFIG_ALLOW_SYSTEM_LIBS=1
