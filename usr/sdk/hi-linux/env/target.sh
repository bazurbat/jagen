#!/bin/sh

# export CFLAGS="-mcpu=cortex-a9 -mfpu=vfpv3-d16 -mfloat-abi=softfp -msoft-float \
# -O2 -fno-strict-aliasing"
export CFLAGS="-O2 -fno-strict-aliasing"
export CXXFLAGS="$CFLAGS"
export ASMFLAGS="$CFLAGS"
export LDFLAGS=""
