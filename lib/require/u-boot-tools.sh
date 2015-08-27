#!/bin/sh

if [ -z "$(command -v mkimage)" ]; then
    error "mkimage command was not found, please install U-Boot tools."
    return 22
fi
