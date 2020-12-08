#!/bin/sh

CFLAGS="${CFLAGS:+$CFLAGS }${toolchain_cflags-}"
CXXFLAGS="${CXXFLAGS:+$CXXFLAGS }${toolchain_cxxflags-}"
LDFLAGS="${LDFLAGS:+$LDFLAGS }${toolchain_ldflags-}"

# Taken from Autoconf's default '-g -O2' and separated by profile.
# Should be a sensible default for other Make-based packages.
case $(pkg_get_build_profile) in
    release)
        pkg__default_cflags='-O2' ;;
    debug)
        pkg__default_cflags='-g' ;;
    release_with_debug)
        pkg__default_cflags='-g -O1' ;;
esac

case ${pkg_build_type:?} in
    cmake)
        # CMake has its own built-in defaults
        ;;
    *)
        CFLAGS="${CFLAGS:+$CFLAGS }${pkg_build_cflags:-$pkg__default_cflags}"
        CXXFLAGS="${CXXFLAGS:+$CXXFLAGS }${pkg_build_cxxflags:-$pkg__default_cflags}"
        LDFLAGS="${LDFLAGS:+$LDFLAGS }${pkg_build_ldflags-}"
        ;;
esac

unset pkg__default_cflags

return 0
