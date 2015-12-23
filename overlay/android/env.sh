#!/bin/sh

# Many Android build and utility scripts assume Bash shell
jagen_shell="/bin/bash"

if in_flags ccache; then
    export USE_CCACHE=1
fi
