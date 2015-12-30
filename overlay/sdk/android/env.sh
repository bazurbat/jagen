#!/bin/sh

jagen_sdk='android'

# Many Android build and utility scripts assume Bash shell
jagen_shell="/bin/bash"

jagen_target_prefix="/system"

if in_flags ccache; then
    export USE_CCACHE=1
fi
