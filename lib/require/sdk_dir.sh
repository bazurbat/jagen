#!/bin/sh

if [ ! "$jagen_sdk_dir" ]; then
    error "jagen_sdk_dir is not defined, please set it to the location of TOP \
directory of HiSilicon SDK."
    return 22
fi
