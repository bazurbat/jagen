#!/bin/sh

if ! [ "$pkg_build_in_source" ]; then
    export KBUILD_OUTPUT="${pkg_build_dir:?}"
fi
