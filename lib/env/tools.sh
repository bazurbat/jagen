#!/bin/sh

export toolsdir="$ja_builddir/tools"
export toolsprefix=""

export PATH="$toolsdir/bin:$PATH"
export LD_LIBRARY_PATH="$toolsdir/lib:$LD_LIBRARY_PATH"

export PKG_CONFIG_PATH="$toolsdir/lib/pkgconfig:$PKG_CONFIG_PATH"
