#!/bin/sh

p_git_is_dirty() { test "$(git status --porcelain)"; }

p_git_clone() {
    p_run git clone --progress "$1" "$2"
}

p_git_fetch() {
    if p_git_is_dirty; then
        warning "$PWD is dirty, not fetching"
    else
        p_run git fetch
    fi
}

p_git_pull() {
    if p_git_is_dirty; then
        warning "$PWD is dirty, not pulling"
    else
        p_run git pull
    fi
}

p_git_checkout() {
    local branch=${1:-master}

    if p_git_is_dirty; then
        warning "$PWD is dirty, not checking out"
    else
        p_run git checkout origin/$branch
    fi
}

p_git_discard() {
    p_run git checkout .
}

p_git_clean() {
    p_run git clean -fxd
}

p_hg_is_dirty() { test "$(hg status)" ; }

p_hg_clone() {
    p_run hg clone "$1" "$2"
}

p_hg_fetch() {
    if p_hg_is_dirty; then
        warning "$PWD is dirty, not fetching"
    else
        p_run hg pull -u
    fi
}

p_hg_pull() {
    if p_hg_is_dirty; then
        warning "$PWD is dirty, not pulling"
    else
        p_run hg pull -u
    fi
}

p_hg_checkout() { :; }

p_hg_discard() {
    p_run hg update -C 
}

p_hg_clean() {
    p_run hg purge --all
}

p__is_dirty() { :; }

p__clone() { :; }

p__fetch() { :; }

p__pull() { :; }

p__checkout() { :; }

p__discard() { :; }

p__clean() { :; }

p_src_kind() {
    local dir="$1"
    if [ -d "${dir}/.git" ]; then
        printf "git"
    elif [ -d "${dir}/.hg" ]; then
        printf "hg"
    fi
}

p_src_is_dirty() {
    local dir=$(realpath "$1")
    local kind=$(p_src_kind "$dir")

    ( cd "$dir" && p_${kind}_is_dirty )
}

p_src_clone() {
    local kind="$1" src="$2" dst="$3"

    p_${kind}_clone "$src" "$dst"
}

p_src_fetch() {
    local dir=$(realpath "$1")
    local kind=$(p_src_kind "$dir")

    ( cd "$dir" && p_${kind}_fetch ) || exit
}

p_src_pull() {
    local dir=$(realpath "$1")
    local kind=$(p_src_kind "$dir")

    ( cd "$dir" && p_${kind}_pull ) || exit
}

p_src_checkout() {
    local dir=$(realpath "$1")
    local kind=$(p_src_kind "$dir")

    ( cd "$dir" && p_${kind}_checkout "$2" ) || exit
}

p_src_discard() {
    local dir=$(realpath "$1")
    local kind=$(p_src_kind "$dir")

    ( cd "$dir" && p_${kind}_discard ) || exit
}

p_src_clean() {
    local dir=$(realpath "$1")
    local kind=$(p_src_kind "$dir")

    ( cd "$dir" && p_${kind}_clean ) || exit
}

p_src_upload() {
    local src=$(realpath "$1") dst=$(realpath "$2")
    local name=$(basename "$src")
    local kind=$(p_src_kind "$src")

    p_run tar -C $(dirname "$src") -cf "$dst/${name}.tar" \
        "$name/${kind:+.$kind}"
}

p_src_download() {
    local src=$(realpath "$1") dst=$(realpath "$2") kind
    local name=$(basename "$src" .tar)

    [ -d "$dst" ] || mkdir -p "$dst"
    p_run tar -C "$dst" -xpf "$src"
    p_src_discard "$dst/$name"
}
