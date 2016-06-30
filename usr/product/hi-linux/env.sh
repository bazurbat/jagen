#!/bin/sh

require kernel

jagen_sdk_src_dir="${jagen_src_dir:?}/hi-sdk"
jagen_sample_src_dir="${jagen_src_dir:?}/hi-sample"

jagen_sdk_tools_dir="${jagen_build_dir:?}/hi-sdk-tools/tools"

jagen_rootfs_staging_dir="$jagen_build_dir/rootfs"
