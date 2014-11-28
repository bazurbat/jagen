#!/bin/sh

pkg_clean() {
    set -- $p_source
    local kind="$1"

    case $kind in
        git|hg)
            if p_in_list "$p_name" "$pkg_source_exclude"; then
                message "pkg source '$p_name' excluded from cleaning"
            elif [ -d "$p_source_dir" ]; then
                p_src_discard "$p_source_dir"
                p_src_clean "$p_source_dir"
            fi
            ;;
    esac

    p_clean_dir "$p_work_dir"
}

pkg_unpack() {
    set -- $p_source
    local kind="$1"
    local src="${2:-$1}"

    case $kind in
        git|hg)
            if p_in_list "$p_name" "$pkg_source_exclude"; then
                message "pkg source '$p_name' excluded from pulling"
            elif [ -d "$p_source_dir" ]; then
                if p_src_is_dirty "$p_source_dir"; then
                    warning "$p_source_dir is dirty, not updating"
                else
                    p_src_checkout "$p_source_dir" "$p_source_branch"
                    p_src_pull "$p_source_dir"
                fi
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

pkg_build_pre() {
    [ -d "$p_build_dir" ] || p_run mkdir -p "$p_build_dir"
    p_run cd "$p_build_dir"
}

pkg_install_pre() {
    # for packages that do not have build stage
    pkg_build_pre
}
