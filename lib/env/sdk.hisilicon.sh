#!/bin/sh

export target_dir="$pkg_build_dir/target"

export target_arch="arm"
export target_system="arm-hisiv200-linux"

export target_bin_dir="$target_dir/bin"

export JAVA_HOME="/usr/lib/jvm/sun-jdk-1.6"
export JAVAC="$JAVA_HOME/bin/javac"
# export _JAVA_OPTIONS="-Xms1024m -Xmx2048m -XX:MaxPermSize=256m"

if in_flags ccache; then
    export USE_CCACHE=1
fi

p_path_prepend "$JAVA_HOME/bin"
