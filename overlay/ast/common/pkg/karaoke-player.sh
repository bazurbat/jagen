#!/bin/sh

jagen_pkg_build_host() {
    pkg_build \
        $(pkg_using_target_board)
}

jagen_pkg_build_target() {
    case $jagen_sdk in
        sigma)
            pkg_build \
                $(pkg_using_sigma_sdk) \
                $(pkg_using_host_chicken) \
                $(pkg_using_target_board)
            ;;
        android)
            pkg_build \
                $(pkg_using_android_toolchain) \
                $(pkg_using_hisilicon_sdk) \
                $(pkg_using_target_board)
            ;;
        hi-linux)
            pkg_build \
                $(pkg_using_hisilicon_sdk) \
                $(pkg_using_host_chicken) \
                $(pkg_using_target_board)
            ;;
        *)
            pkg_build \
                $(pkg_using_target_board)
            ;;
    esac
}
