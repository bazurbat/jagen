#!/bin/sh

p_in_list() { echo "$2" | grep -qw "$1"; }

p_list_remove() {
    local list item S
    list="$1" item="$2" S=${3:-:}

    echo "$list" |
    sed "s|\(.*${S}\?\)${item}\(.*${S}\?\)|\1\2|g" |
    sed "s|$S$S|$S|g"
}

p_path_prepend() {
    local item="$1"
    PATH=${item}:$(p_list_remove "$PATH" "$item")
}
