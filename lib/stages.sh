#!/bin/sh

jagen_pkg_clean() {
    set -- $pkg_source
    local kind="$1"

    case $kind in
        git|hg)
            if in_list "$pkg_name" $jagen_source_exclude; then
                message "pkg source '$pkg_name' excluded from cleaning"
            elif [ -d "$pkg_source_dir" ]; then
                _jagen src clean "$pkg_name"
            fi
            ;;
    esac

    pkg_clean_dir "$pkg_work_dir"
}

jagen_pkg_unpack() {
    set -- $pkg_source
    local kind="$1"
    local src="${2:-$1}"

    [ "$pkg_source" ] || return 0

    case $kind in
        git|hg)
            if in_flags "offline"; then
                message "Offline mode, not checking $pkg_name"
            elif in_list "$pkg_name" $jagen_source_exclude; then
                message "pkg source '$pkg_name' excluded from pulling"
            elif [ -d "$pkg_source_dir" ]; then
                if _jagen src dirty "$pkg_name"; then
                    warning "$pkg_source_dir is dirty, not updating"
                else
                    _jagen src update "$pkg_name"
                fi
            else
                _jagen src clone "$pkg_name"
            fi
            ;;
        *)
            pkg_run tar -C "$pkg_work_dir" -xf "$src"
            ;;
    esac
}

default_patch() {
    if [ ! -x "$pkg_source_dir/configure" -a -x "$pkg_source_dir/autogen.sh" ]; then
        "$pkg_source_dir/autogen.sh"
    fi
}

jagen_pkg_configure() {
    if [ "$pkg_with_provided_libtool" ]; then
        pkg_run_autoreconf
    fi
}

jagen_pkg_build_pre() {
    [ -d "$pkg_build_dir" ] || pkg_run mkdir -p "$pkg_build_dir"
    pkg_run cd "$pkg_build_dir"
}

default_build() {
    if [ -x "$pkg_source_dir/configure" ]; then
        pkg_run "$pkg_source_dir/configure" \
            --host="$pkg_system" \
            --prefix="$pkg_prefix" \
            $pkg_options
        pkg_run make
    fi
}

jagen_pkg_install_pre() {
    # for packages that do not have build stage
    pkg_build_pre
}

default_install() {
    pkg_run make DESTDIR="$pkg_dest_dir" install

    for name in $pkg_libs; do
        pkg_fix_la "$pkg_dest_dir$pkg_prefix/lib/lib${name}.la" "$pkg_dest_dir"
    done
}

jagen_pkg_patch() { default_patch; }

jagen_pkg_build() { default_build; }

jagen_pkg_install() { default_install; }
