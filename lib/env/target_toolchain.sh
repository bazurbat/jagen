#!/bin/sh

: ${jagen_target_system:?}

export AR="${jagen_target_system}-ar"
export AS="${jagen_target_system}-as"
export CC="${jagen_target_system}-gcc"
export CXX="${jagen_target_system}-g++"
export CPP="${jagen_target_system}-cpp"
