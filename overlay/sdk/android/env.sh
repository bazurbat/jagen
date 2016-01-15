#!/bin/sh

jagen_sdk='android'

# Many Android build and utility scripts assume Bash shell
jagen_shell="/bin/bash"

jagen_target_prefix="/system"

if in_flags ccache; then
    export USE_CCACHE=1
fi

if [ "$jagen_android_product" ]; then
    export jagen_android_out_dir="$jagen_sdk_dir/out/target/product/$jagen_android_product"
fi
