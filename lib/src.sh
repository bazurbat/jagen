#!/bin/sh

pkg_git_is_dirty() { test "$(git status --porcelain)"; }

pkg_git_head() { pkg_run git rev-parse HEAD; }

pkg_git_clone() {
    pkg_run git clone \
        --progress \
        --depth 1 \
        --no-single-branch \
        --no-checkout \
        "$1" "$2"
}

pkg_git_fetch() { pkg_run git fetch --progress -np; }

pkg_git_pull() { pkg_run git pull --progress --ff-only; }

pkg_git_checkout() {
    local branch=${1:-master}

    if [ "$(git branch --list $branch)" ]; then
        pkg_run git checkout "$branch"
    else
        pkg_run git checkout -b "$branch" -t origin/$branch
    fi
}

pkg_git_discard() { pkg_run git checkout .; }

pkg_git_clean() { pkg_run git clean -fxd; }

pkg_hg_is_dirty() { test "$(hg status)"; }

pkg_hg_head() { pkg_run hg id -i; }

pkg_hg_clone() { pkg_run hg clone -r tip "$1" "$2"; }

pkg_hg_fetch() { pkg_run hg pull; }

pkg_hg_pull() { pkg_run hg pull -u; }

pkg_hg_checkout() { pkg_run hg update -c; }

pkg_hg_discard() { pkg_run hg update -C; }

pkg_hg_clean() { pkg_run hg purge --all; }

pkg__is_dirty() { :; }

pkg__clone() { :; }

pkg__fetch() { :; }

pkg__pull() { :; }

pkg__checkout() { :; }

pkg__discard() { :; }

pkg__clean() { :; }

pkg_src_kind() {
    local dir="$1"
    if [ -d "${dir}/.git" ]; then
        printf "git"
    elif [ -d "${dir}/.hg" ]; then
        printf "hg"
    fi
}

pkg_src_is_dirty() {
    local dir="$1"
    local kind=$(pkg_src_kind "$dir")

    ( cd "$dir" && p_${kind}_is_dirty )
}

pkg_src_head() {
    local dir="$1"
    local kind=$(pkg_src_kind "$dir")

    (cd "$dir" && p_${kind}_head ) || exit
}

pkg_src_clone() {
    local kind="$1" src="$2" dst="$3"

    p_${kind}_clone "$src" "$dst"
}

pkg_src_fetch() {
    local dir="$1"
    local kind=$(pkg_src_kind "$dir")

    ( cd "$dir" && p_${kind}_fetch ) || exit
}

pkg_src_pull() {
    local dir="$1"
    local kind=$(pkg_src_kind "$dir")

    ( cd "$dir" && p_${kind}_pull ) || exit
}

pkg_src_checkout() {
    local dir="$1"
    local kind=$(pkg_src_kind "$dir")

    ( cd "$dir" && p_${kind}_checkout "$2" ) || exit
}

pkg_src_discard() {
    local dir="$1"
    local kind=$(pkg_src_kind "$dir")

    ( cd "$dir" && p_${kind}_discard ) || exit
}

pkg_src_clean() {
    local dir="$1"
    local kind=$(pkg_src_kind "$dir")

    ( cd "$dir" && p_${kind}_clean ) || exit
}
