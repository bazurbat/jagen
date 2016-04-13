#!/bin/sh

jagen_toolchain_env_dir=$(dirname "$1")

jagen_target_arch="arm"
jagen_target_system="arm-hisiv200-linux"

jagen_toolchain_bin_dir="${jagen_root}/bin"
jagen_toolchain_sysroot="${jagen_toolchain_dir}/target"

jagen_sysdeps_cfg="${jagen_toolchain_env_dir:?}/sysdeps.cfg"
if ! [ -d "$jagen_sysdeps_cfg" ]; then
    jagen_sysdeps_cfg=''
fi

jagen_toolchain_install_runtime() {
    local dest="${1:?}"
    local abi="armv5te_soft"

    pkg_run rsync -a \
        "$jagen_toolchain_dir/target/$abi/lib/" \
        "$dest/lib"
    pkg_run rsync -a \
        "$jagen_toolchain_dir/arm-hisiv200-linux-gnueabi/lib/$abi/" \
        "$dest/lib"
}
