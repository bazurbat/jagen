#!/bin/sh

pkg_unpack() {
    local i=0 j=0

    message start
    while [ $((++i)) -lt 100000 ]; do
        if [ $((i % 10000)) = 0 ]; then
            : $((j++))
            sleep 0.1
        fi
        echo $j $i
    done
    message finish
}
