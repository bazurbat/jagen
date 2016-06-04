#!/bin/sh

: ${jagen_android_product:?}

message "Changing current directory to: $jagen_sdk_dir"
cd "$jagen_sdk_dir" || return

message "Preparing the environment"
. ./build/envsetup.sh || return

message "Preparing product: $jagen_android_product"
lunch "${jagen_android_product}-eng" || return
