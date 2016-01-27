#!/bin/sh

jagen_pkg_build_host() {
    default_build \
        $(pkg_using_target_board)
}

jagen_pkg_build_target() {
    case $jagen_sdk in
        sigma)
            default_build \
                $(pkg_using_sigma_sdk) \
                $(pkg_using_host_chicken) \
                $(pkg_using_target_board)
            ;;
        android)
            default_build \
                $(pkg_using_android_toolchain) \
                $(pkg_using_hisilicon_sdk) \
                $(pkg_using_target_board)
            ;;
        hi-linux)
            default_build \
                $(pkg_using_hisilicon_sdk) \
                $(pkg_using_host_chicken) \
                $(pkg_using_target_board)
            ;;
        *)
            default_build \
                $(pkg_using_target_board)
            ;;
    esac
}
