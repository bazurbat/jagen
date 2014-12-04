#!/bin/sh

p_in_list() { echo "$2" | grep -qw "$1"; }

p_list_remove() {
    local list=$1 item=$2 s=${3:-:}
    echo "$list" | sed -e "s|${s}\?${item}${s}\?||g"
}

p_path_prepend() {
    local item="$1"
    PATH=${item}:$(p_list_remove "$PATH" "$item" :)
}
