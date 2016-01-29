#!/bin/sh

jagen_pkg_build_target() {
    case $jagen_sdk in
        android)
            pkg_build \
                $(pkg_using_android_toolchain) \
                $(pkg_using_hisilicon_sdk)
            ;;
        hi-linux)
            pkg_build \
                $(pkg_using_hisilicon_sdk)
            ;;
    esac
}
