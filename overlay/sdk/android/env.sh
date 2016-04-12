#!/bin/sh

jagen_sdk='android'

# Many Android build and utility scripts assume Bash shell
jagen_shell="/bin/bash"

: ${jagen_sdk_dir:?}
: ${jagen_android_product:?}

# set output directory of Android build system
# export OUT_DIR="${jagen_out_dir:?}/android"

jagen_sdk_staging_dir="$OUT_DIR/target/product/$jagen_android_product"

# for out of tree Linux kernel modules
export KDIR="$jagen_sdk_staging_dir/obj/KERNEL_OBJ"

if in_flags ccache; then
    export USE_CCACHE=1
fi
