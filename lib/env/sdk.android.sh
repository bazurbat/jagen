#!/bin/sh

# Many Android build and utility scripts assume Bash shell
pkg_shell="/bin/bash"

target_system="arm-linux-androideabi"
target_prefix="/system"

target_arch="arm"
target_platform="${target_platform:-android-17}"
target_toolchain="${target_toolchain:-${target_system}-4.6}"

toolchain_bin_dir="${target_dir}/${target_toolchain}/bin"

export JAVA_HOME="/usr/lib/jvm/sun-jdk-1.6"
export JAVAC="$JAVA_HOME/bin/javac"
# export _JAVA_OPTIONS="-Xms1024m -Xmx2048m -XX:MaxPermSize=256m"

if in_flags ccache; then
    export USE_CCACHE=1
fi

add_PATH "$JAVA_HOME/bin"

make_toolchain() {
    : ${jagen_toolchain_dir:?}

    rm -fr "$toolchain_bin_dir"
    mkdir -p "$toolchain_bin_dir"

    bash "$jagen_toolchain_dir/build/tools/make-standalone-toolchain.sh" \
        --system="linux-x86_64" \
        --platform="$target_platform" \
        --toolchain="$target_toolchain" \
        --install-dir="${target_dir}/${target_toolchain}"
}
