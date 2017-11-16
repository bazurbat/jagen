#!/bin/sh

jagen_target_arch="aarch64"
jagen_target_system="${jagen_target_arch}-linux-gnu"

jagen_target_toolchain="gcc-linaro-5.3_aarch64"
jagen_target_toolchain_dir="${jagen_toolchains_dir:?}/gcc-linaro-5.3.1-2016.05-x86_64_aarch64-linux-gnu"

# jagen_target_cflags="-march=armv7-a -mcpu=cortex-a9 -mfpu=vfpv3-d16 -mfloat-abi=softfp"
