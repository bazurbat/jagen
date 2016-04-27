#!/bin/sh

jagen_pkg_configure_target() {
    case $jagen_sdk in
        hi-linux)
            pkg_configure \
                $(pkg_using_hisilicon_sdk) \
                $(pkg_using_target_board)
            ;;
        *)
            die "unsupported sdk: $jagen_sdk"
            ;;
    esac
}
