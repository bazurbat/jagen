#!/bin/sh

export JAVA_HOME="/usr/lib/jvm/sun-jdk-1.6"
export JAVAC="$JAVA_HOME/bin/javac"
# export _JAVA_OPTIONS="-Xms1024m -Xmx2048m -XX:MaxPermSize=256m"

export TOP="$sdk_top_dir"

if in_flags ccache; then
    export USE_CCACHE=1
fi

add_PATH "$JAVA_HOME/bin"
