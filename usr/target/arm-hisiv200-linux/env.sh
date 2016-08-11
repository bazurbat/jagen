#!/bin/sh

jagen_target_system="arm-hisiv200-linux-gnueabi"

jagen_target_arch="arm"
jagen_target_cpu="cortex-a9"

jagen_target_toolchain="arm-hisiv200-linux"
jagen_target_toolchain_dir="${jagen_toolchains_dir:?}/${jagen_target_toolchain:?}"

jagen_target_cflags="-march=armv7-a -mcpu=cortex-a9 -mfpu=vfpv3-d16 -mfloat-abi=softfp"

toolchain_install_ldconfig() {
    local lib_path="$(toolchain_find_path libc.so.6)"
    local ldconfig_path="$(real_path "${lib_path:?}/../sbin")/ldconfig"

    if [ -x "$ldconfig_path" ]; then
        pkg_run rsync -vtp --chmod=0755 \
            "$ldconfig_path" \
            "$pkg_install_dir/sbin/ldconfig"
    else
        die "Could not find ldconfig"
    fi
}
