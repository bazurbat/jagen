#!/bin/sh

jagen_sdk="sigma"
jagen_toolchain="mipsel-linux-gnu"
jagen_vendor="ast"

jagen_init_toolchain_dir='$jagen_base_dir/toolchain/mips-2012.03'
jagen_init_sdk_dir='$jagen_src_dir/sigma-mrua'

jagen_target_board="${jagen_target_board:-ast100}"