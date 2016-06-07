#!/bin/sh

jagen_pkg_configure_host() {
    local IFS="$jagen_IFS" S="$jagen_FS" A=

    # If an executable has RUNPATH set, LD_LIBRARY_PATH is searched first,
    # which is the case with csi on gentoo. This prevents automatic dependency
    # generation to work once compiled libchicken becomes available in the
    # build directory.

    unset LD_LIBRARY_PATH

    # Ignore already installed CHICKEN in the pacakge prefix for clean rebuild.
    # Newer CMake has more convenient CMAKE_FIND_NO_INSTALL_PREFIX for the same
    # purpose, but we are stuck with 2.8.12 for now.

    A="-DCMAKE_SYSTEM_IGNORE_PATH=$pkg_prefix"

    if in_flags new_chicken; then
        A="$A$S-DCHICKEN_COMPILER=$jagen_host_dir/bin/chicken-boot"
        A="$A$S-DCHICKEN_INTERPRETER=/usr/bin/csi"
    fi

    pkg_configure $A
}

jagen_pkg_configure_target() {
    case $jagen_sdk in
        sigma)
            pkg_configure \
                -DCMAKE_SYSTEM_PROCESSOR="mips32" \
                $(pkg_using_install_prefix) \
                $(pkg_using_host_chicken)
            ;;
        android)
            pkg_configure \
                $(pkg_using_install_prefix) \
                $(pkg_using_android_toolchain) \
                $(pkg_using_host_chicken)
            ;;
        *)
            pkg_configure \
                $(pkg_using_install_prefix) \
                $(pkg_using_host_chicken)
            ;;
    esac
}

jagen_pkg_install_target() {
    export DESTDIR="$pkg_sysroot"
    pkg_install
}
