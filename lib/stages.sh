#!/bin/sh

pkg_clean() {
    set -- $p_source
    local kind="$1"

    case $kind in
        git|hg)
            if in_list "$p_name" $jagen_source_exclude; then
                message "pkg source '$p_name' excluded from cleaning"
            elif [ -d "$p_source_dir" ]; then
                _jagen src clean "$p_name"
            fi
            ;;
    esac

    pkg_clean_dir "$p_work_dir"
}

pkg_unpack() {
    set -- $p_source
    local kind="$1"
    local src="${2:-$1}"

    [ "$p_source" ] || return 0

    case $kind in
        git|hg)
            if in_flags "offline"; then
                message "Offline mode, not checking $p_name"
            elif in_list "$p_name" $jagen_source_exclude; then
                message "pkg source '$p_name' excluded from pulling"
            elif [ -d "$p_source_dir" ]; then
                if p_src_is_dirty "$p_source_dir"; then
                    warning "$p_source_dir is dirty, not updating"
                else
                    _jagen src update "$p_name"
                fi
            else
                _jagen src clone "$p_name"
            fi
            ;;
        *)
            pkg_run tar -C "$p_work_dir" -xf "$src"
            ;;
    esac
}

default_patch() {
    if [ ! -x "$p_source_dir/configure" -a -x "$p_source_dir/autogen.sh" ]; then
        "$p_source_dir/autogen.sh"
    fi
}

pkg_configure() {
    if [ "$p_with_provided_libtool" ]; then
        pkg_run_autoreconf
    fi
}

pkg_build_pre() {
    [ -d "$p_build_dir" ] || pkg_run mkdir -p "$p_build_dir"
    pkg_run cd "$p_build_dir"
}

default_build() {
    if [ -x "$p_source_dir/configure" ]; then
        pkg_run "$p_source_dir/configure" \
            --host="$p_system" \
            --prefix="$p_prefix" \
            $p_options
        pkg_run make
    fi
}

pkg_install_pre() {
    # for packages that do not have build stage
    pkg_build_pre
}

default_install() {
    pkg_run make DESTDIR="$p_dest_dir" install

    for name in $p_libs; do
        pkg_fix_la "$p_dest_dir$p_prefix/lib/lib${name}.la" "$p_dest_dir"
    done
}

pkg_patch() { default_patch; }

pkg_build() { default_build; }

pkg_install() { default_install; }
