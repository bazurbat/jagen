#!/bin/sh

jagen_pkg_configure_target() {
    case $jagen_sdk in
        android)
            pkg_configure \
                $(pkg_using_android_toolchain) \
                $(pkg_using_hisilicon_sdk)
            ;;
        hi-linux)
            pkg_configure \
                $(pkg_using_hisilicon_sdk)
            ;;
    esac
}
