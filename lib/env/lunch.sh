#!/bin/sh

require bash || return
require sdk_dir || return
require u-boot-tools || return

use_toolchain target

message "Changing current directory to: $jagen_sdk_dir"
cd "$jagen_sdk_dir" || return

message "Preparing the environment"
. ./build/envsetup.sh || return

: ${target_product:?}

message "Preparing product: $target_product"
lunch "${target_product}-eng" || return
