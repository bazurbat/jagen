#!/bin/sh

jagen_sdk_dir="${jagen_src_dir:?}/hi-sdk"

jagen_kernel_dir="${jagen_src_dir:?}/hi-kernel"
jagen_kernel_config='ast2xx_hi3719cv100_defconfig'
jagen_kernel_image='uImage'

export KERNEL_SRC="$jagen_kernel_dir"
