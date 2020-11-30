#!/bin/sh

if [ "$toolchain_cflags" ]; then
    CFLAGS="${CFLAGS:+$CFLAGS }$toolchain_cflags"
fi
if [ "$toolchain_cxxflags" ]; then
    CXXFLAGS="${CXXFLAGS:+$CXXFLAGS }$toolchain_cxxflags"
fi
if [ "$toolchain_ldflags" ]; then
    LDFLAGS="${LDFLAGS:+$LDFLAGS }$toolchain_ldflags"
fi
