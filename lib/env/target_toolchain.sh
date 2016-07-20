#!/bin/sh

: ${jagen_target_system:?}

export AR="${jagen_target_system}-ar"
export AS="${jagen_target_system}-as"
export CC="${jagen_target_system}-gcc"
export CPP="${jagen_target_system}-cpp"
export CXX="${jagen_target_system}-g++"
export LD="${jagen_target_system}-ld"
export NM="${jagen_target_system}-nm"
export OBJCOPY="${jagen_target_system}-objcopy"
export OBJDUMP="${jagen_target_system}-objdump"
export RANLIB="${jagen_target_system}-ranlib"
export STRIP="${jagen_target_system}-strip"

