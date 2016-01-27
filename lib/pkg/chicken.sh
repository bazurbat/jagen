#!/bin/sh

jagen_pkg_build_host() {
    # Ignore already installed CHICKEN in the pacakge prefix for clean rebuild.
    # Newer CMake has more convenient CMAKE_FIND_NO_INSTALL_PREFIX for the same
    # purpose, but we are stuck with 2.8.12 for now.

    default_build -DCMAKE_SYSTEM_IGNORE_PATH="$pkg_prefix"
}

jagen_pkg_build_target() {
    case $jagen_sdk in
        sigma)
            default_build \
                -DCMAKE_SYSTEM_PROCESSOR="mips32" \
                $(pkg_using_host_chicken)
            ;;
        android)
            default_build \
                $(pkg_using_android_toolchain) \
                $(pkg_using_host_chicken)
            ;;
        *)
            default_build \
                $(pkg_using_host_chicken)
            ;;
    esac
}
