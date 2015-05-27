#!/bin/sh

export target_dir="$pkg_build_dir/target"
export target_prefix="/system"

export target_arch="arm"
# export target_system="arm-hisiv200-linux"
export target_system="arm-linux-androideabi"

export target_bin_dir="$target_dir/bin"

export sdk_firmware_dir="$pkg_build_dir/firmware"

export JAVA_HOME="/usr/lib/jvm/sun-jdk-1.6"
export JAVAC="$JAVA_HOME/bin/javac"
# export _JAVA_OPTIONS="-Xms1024m -Xmx2048m -XX:MaxPermSize=256m"

export TOP="$sdk_top_dir"

if in_flags ccache; then
    export USE_CCACHE=1
fi

p_path_prepend "$JAVA_HOME/bin"
