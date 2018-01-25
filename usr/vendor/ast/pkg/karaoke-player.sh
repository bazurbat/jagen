#!/bin/sh

jagen_pkg_configure_host() {
    pkg_configure \
        $(pkg_using_target_board)
}

jagen_pkg_configure_target() {
    case $jagen_sdk in
        sigma)
            pkg_configure \
                -DSIGMA_SDK_DIR="${mrua_dir:?}" \
                -DSIGMA_ROOTFS_DIR="${rootfs_dir:?}" \
                $(pkg_using_host_chicken) \
                $(pkg_using_target_board)
            ;;
        android)
            pkg_configure \
                $(pkg_using_android_toolchain) \
                $(pkg_using_hisilicon_sdk) \
                $(pkg_using_target_board)
            ;;
        hi-linux)
            pkg_configure \
                $(pkg_using_hisilicon_sdk) \
                $(pkg_using_host_chicken) \
                $(pkg_using_target_board)
            ;;
        *)
            pkg_configure \
                $(pkg_using_target_board)
            ;;
    esac
}
