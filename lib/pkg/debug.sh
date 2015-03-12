#!/bin/sh

pkg_unpack() {
    local i=0
    while [ $((++i)) -lt 100000 ]; do
        echo $i
    done
}
