#!/bin/sh

jagen_sdk='hi-linux'

jagen_shell='/bin/bash'

jagen_kernel_release="3.4.67_s40"
jagen_kernel_dir="$jagen_src_dir/hi-kernel"
jagen_kernel_modules_install_dir="/lib/modules/$jagen_kernel_release"
jagen_kernel_extra_modules_install_dir="$jagen_kernel_modules_install_dir/extra"

export KDIR="$jagen_kernel_dir"
