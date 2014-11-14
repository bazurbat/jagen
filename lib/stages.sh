#!/bin/sh

pkg_clean() {
    set -- $p_source
    local kind="$1"

    case $kind in
        git|hg)
            p_src_clean "$p_source_dir"
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
            if [ -d "$p_source_dir" ]; then
                p_src_pull "$p_source_dir"
            else
                p_src_clone "$kind" "$src" "$p_source_dir"
                p_src_checkout "$p_source_dir" "$p_source_branch"
            fi
            ;;
        *)
            p_run tar -C "$p_work_dir" -xf "$src"
            ;;
    esac
}
