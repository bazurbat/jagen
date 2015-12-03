#!/bin/sh

jagen_sdk='hi-linux'

jagen_target_system="arm-hisiv200-linux"

jagen_target_toolchain_dir="${jagen_target_dir}"
jagen_toolchain_sysroot="${jagen_toolchain_dir}/target"
jagen_toolchain_prefix="${jagen_target_toolchain_dir}/bin/${jagen_target_system}-"
