#!/bin/sh

if [ ! "$BASH" ]; then
    error "Android build requires Bash shell, please run it and source the \
environment again."
    return 22
fi
