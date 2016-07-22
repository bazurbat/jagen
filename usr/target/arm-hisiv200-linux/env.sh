#!/bin/sh

jagen_target_system="arm-hisiv200-linux-gnueabi"

jagen_target_arch="arm"
jagen_target_cpu="cortex-a9"

jagen_target_toolchain="arm-hisiv200-linux"
jagen_target_toolchain_dir="${jagen_toolchains_dir:?}/${jagen_target_toolchain:?}"

jagen_target_cflags="-march=armv7-a -mcpu=cortex-a9 -mfpu=vfpv3-d16 -mfloat-abi=softfp"
