#!/bin/sh

. "$jagen_lib_dir/common.sh"

test_in_list() {
    local value="$1"; [ $# -ge 1 ] && shift

    if in_list "$value" "$@"; then
        echo "$value	+ '$*'"
    else
        echo "$value	- '$*'"
    fi
}

test_list_remove() {
    local value="$1"; shift

    printf "%s - %s:\n" "$*" "$value"
    list_remove ' ' "$value" "$@"
}

list1='abd adge br fjr lkw'
list2=''
list3='abd'

echo "== in_list =="

test_in_list ab  $list1
test_in_list lkw $list1
test_in_list abd $list1
test_in_list adg $list1
test_in_list br  $list1
test_in_list aa  $list2
test_in_list ab  $list3
test_in_list z   $list3
test_in_list abd $list3

echo "== list_remove =="

test_list_remove ab  $list1
test_list_remove lkw $list1
test_list_remove abd $list1
test_list_remove adg $list1
test_list_remove br  $list1
test_list_remove aa  $list2
test_list_remove ab  $list3
test_list_remove z   $list3
test_list_remove abd $list3

echo "== add_PATH =="

add_PATH aaa
add_PATH bbb
add_PATH aaa

echo $PATH
