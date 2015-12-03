#!/bin/sh

jagen_sdk='hi-linux'

jagen_target_system="arm-hisiv200-linux"

toolchain_dir="${jagen_target_dir}"
toolchain_sysroot="${jagen_toolchain_dir}/target"
toolchain_prefix="${toolchain_dir}/bin/${jagen_target_system}-"
