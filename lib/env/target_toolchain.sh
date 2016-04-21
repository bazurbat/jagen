#!/bin/sh

: ${jagen_target_system:?}

export AR="${jagen_target_system}-ar"
export CC="${jagen_target_system}-gcc"
export CXX="${jagen_target_system}-g++"
export STRIP="${jagen_target_system}-strip"
