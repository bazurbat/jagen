#!/bin/sh

target_prefix="/system"

target_system="arm-linux-androideabi"

target_arch="arm"
target_platform="${target_platform:-android-17}"
target_toolchain="${target_toolchain:-${target_system}-4.6}"

target_bin_dir="$target_dir/$target_toolchain"

sdk_firmware_dir="$pkg_build_dir/firmware"

export JAVA_HOME="/usr/lib/jvm/sun-jdk-1.6"
export JAVAC="$JAVA_HOME/bin/javac"
# export _JAVA_OPTIONS="-Xms1024m -Xmx2048m -XX:MaxPermSize=256m"

export TOP="$sdk_top_dir"

if in_flags ccache; then
    export USE_CCACHE=1
fi

add_PATH "$JAVA_HOME/bin"
