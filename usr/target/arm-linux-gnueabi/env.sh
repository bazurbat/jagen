#!/bin/sh

jagen_target_arch="arm"
jagen_target_system="${jagen_target_arch}-linux-gnueabi"

jagen_target_toolchain="gcc-linaro-4.8"
jagen_target_toolchain_dir="${jagen_toolchains_dir:?}/gcc-linaro-4.8-2015.06-x86_64_arm-linux-gnueabi"

jagen_target_cflags="-march=armv7-a -mcpu=cortex-a9 -mfpu=vfpv3-d16 -mfloat-abi=softfp"
