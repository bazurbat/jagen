#!/bin/sh

# these are used by Kbuild packages to setup cross-compiling
export ARCH="$pkg_build_arch"
export CROSS_COMPILE="$pkg_toolchain_prefix"
