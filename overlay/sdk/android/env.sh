#!/bin/sh

jagen_sdk='android'

# Many Android build and utility scripts assume Bash shell
jagen_shell="/bin/bash"

jagen_target_prefix="/system"

: ${jagen_sdk_dir:?}
: ${jagen_android_product:?}

jagen_sdk_staging_dir="$jagen_sdk_dir/out/target/product/$jagen_android_product"

if in_flags ccache; then
    export USE_CCACHE=1
fi
