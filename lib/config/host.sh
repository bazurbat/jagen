#!/bin/sh
# shellcheck disable=2154

pkg_install_prefix=$jagen_host_dir

export PKG_CONFIG_PATH="$pkg_install_prefix/lib/pkgconfig"
