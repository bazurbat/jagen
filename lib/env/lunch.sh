#!/bin/sh

require bash || return
require sdk_dir || return
require u-boot-tools || return

: ${jagen_target_product:?}

use_env target

message "Changing current directory to: $jagen_sdk_dir"
cd "$jagen_sdk_dir" || return

message "Preparing the environment"
. ./build/envsetup.sh || return

message "Preparing product: $jagen_target_product"
lunch "${jagen_target_product}-eng" || return
