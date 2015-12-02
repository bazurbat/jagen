#!/bin/sh

jagen_sdk='hi-linux'

target_system="arm-hisiv200-linux"

toolchain_dir="${jagen_target_dir}"
toolchain_sysroot="${jagen_toolchain_dir}/target"
toolchain_prefix="${toolchain_dir}/bin/${target_system}-"
