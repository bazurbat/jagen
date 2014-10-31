#!/bin/sh

p_dist_dir="$ja_root/dist/$ja_sdk"

p_git_clone() {
    p_run git clone --progress "$1" "$2"
}

p_git_update() {
    [ "$(git status --porcelain)" ] || p_run git pull --progress
}

p_git_checkout() {
    [ "$(git status --porcelain)" ] || p_run git checkout "${1:-master}"
}

p_hg_clone() {
    p_run hg clone "$1" "$2"
}

p_hg_update() {
    [ "$(hg status)" ] || p_run hg pull -u
}

p_hg_checkout() {
    :
}

p_scm_update() {
    local kind="$1" src="$2" dst="$3" branch="$4"

    if [ -d "$dst" ]; then
        p_run cd "$dst"
        p_${kind}_update
    else
        p_${kind}_clone "$src" "$dst"
        p_run cd "$dst"
        p_${kind}_checkout "$branch"
    fi
}

p_scm_clean() {
    local kind="$1" dir="$2"

    [ -d "$dir" ] || return 0

    p_run cd "$dir"

    case $kind in
        git)
            p_run git clean -fxd
            ;;
        hg)
            p_run hg purge --all
            ;;
    esac
}

pkg_clean() {
    set -- $p_source
    local kind="$1"

    case $kind in
        git|hg)
            p_scm_clean "$kind" "$p_source_dir"
            ;;
    esac

    [ -d "$p_work_dir" ] && p_run rm -rf "$p_work_dir"
    [ -d "$p_work_dir" ] || p_run mkdir -p "$p_work_dir"
}

pkg_unpack() {
    set -- $p_source
    local kind="$1"
    local src="${2:-$1}"

    case $kind in
        git|hg)
            p_scm_update "$kind" "$src" "$p_source_dir" "$p_source_branch"
            ;;
        *)
            p_run tar -C "$p_work_dir" -xf "$src"
            ;;
    esac
}
