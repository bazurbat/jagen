#!/bin/sh

jagen_target_arch="arm"
jagen_target_system="arm-hisiv200-linux"

ARCH_CFLAGS="-march=armv7-a -mcpu=cortex-a9 -mfpu=vfpv3-d16 -mfloat-abi=softfp"
