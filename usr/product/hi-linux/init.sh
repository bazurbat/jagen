#!/bin/sh

jagen_sdk="hi-linux"
jagen_toolchain="arm-hisiv200-linux"
jagen_vendor="ast"

jagen_init_toolchain_dir='$jagen_base_dir/toolchain/arm-hisiv200-linux'
jagen_init_sdk_dir='$jagen_src_dir/hi-sdk'

jagen_target_board="${jagen_target_board:-ast2xx}"

ARCH_CFLAGS="-march=armv7-a -mcpu=cortex-a9 -mfpu=vfpv3-d16 -mfloat-abi=softfp"
