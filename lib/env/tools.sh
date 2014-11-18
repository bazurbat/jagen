#!/bin/sh

export tools_dir="$pkg_build_dir/tools"
export tools_prefix=""

export PATH="$tools_dir/bin:$PATH"
export LD_LIBRARY_PATH="$tools_dir/lib:$LD_LIBRARY_PATH"

export PKG_CONFIG_PATH="$tools_dir/lib/pkgconfig:$PKG_CONFIG_PATH"
