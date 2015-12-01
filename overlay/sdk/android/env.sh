#!/bin/sh

# Many Android build and utility scripts assume Bash shell
jagen_shell="/bin/bash"

target_system="arm-linux-androideabi"
jagen_target_prefix="/system"

target_arch="arm"
target_platform="${target_platform:-android-17}"
target_toolchain="${target_toolchain:-${target_system}-4.6}"

toolchain_dir="${jagen_target_dir}/${target_toolchain}"
toolchain_bin_dir="${toolchain_dir}/bin"

export JAVA_HOME="/usr/lib/jvm/sun-jdk-1.6"
export JAVAC="$JAVA_HOME/bin/javac"
# export _JAVA_OPTIONS="-Xms1024m -Xmx2048m -XX:MaxPermSize=256m"

if in_flags ccache; then
    export USE_CCACHE=1
fi

if [ "$jagen_sdk_dir" -a -d "$jagen_sdk_dir" ]; then
    sdk_out_dir="$jagen_sdk_dir/out/target/product/Hi3719CV100"
    # adb uses this
    export ANDROID_PRODUCT_OUT="$sdk_out_dir"
fi

add_PATH "$JAVA_HOME/bin"
