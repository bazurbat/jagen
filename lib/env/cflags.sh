#!/bin/sh

if [ "${toolchain_env_CFLAGS+!}" ]; then
    : ${toolchain_env_CXXFLAGS=$toolchain_env_CFLAGS}
fi
if [ "${pkg_env_CFLAGS+!}" ]; then
    : ${pkg_env_CXXFLAGS=$pkg_env_CFLAGS}
fi

pkg_env_CFLAGS="$toolchain_env_CFLAGS $CFLAGS $pkg_env_CFLAGS"
pkg_env_CXXFLAGS="$toolchain_env_CXXFLAGS $CXXFLAGS $pkg_env_CXXFLAGS"
pkg_env_LDFLAGS="$toolchain_env_LDFLAGS $LDFLAGS $pkg_env_LDFLAGS"
