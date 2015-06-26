#!/bin/sh

make_android_toolchain() {
    [ "$android_ndk_dir"       ] || die "android_ndk_dir is not set"
    [ -d "$android_ndk_dir"    ] || die "android_ndk_dir '$android_ndk_dir' is not exists"
    [ "$android_toolchain"     ] || die "android_toolchain is not set"
    [ "$android_platform"      ] || die "android_platform is not set"
    [ "$android_toolchain_dir" ] || die "android_toolchain_dir is not set"

    "$android_ndk_dir/build/tools/make-standalone-toolchain.sh" \
        --toolchain="$android_toolchain" \
        --platform="$android_platform" \
        --install-dir="$android_toolchain_dir"
}

generate_toolchain_wrappers() {
    case $pkg_sdk in
        android)
            make_android_toolchain
            ;;
        *)
            die "Unsupported SDK: $pkg_sdk"
            ;;
    esac
}
