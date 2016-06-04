#!/bin/sh

jagen_sdk="android"
jagen_toolchain="arm-linux-androideabi"
jagen_vendor="ast"

jagen_target_platform="android-17"
jagen_android_product="Hi3719CV100"

jagen_init_toolchain_dir='$jagen_base_dir/toolchain/android-ndk-r10d'
jagen_init_sdk_dir='$jagen_src_dir/android'

jagen_target_board="${jagen_target_board:-ast2xx}"
