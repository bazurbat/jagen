#!/bin/sh

pkg_patch() {
    local IFS; unset IFS
    if is_function jagen_stage_apply_patches; then
        if [ "$pkg_source_exclude" ]; then
            message "not patching $pkg_name: the source is excluded"
        else
            jagen_stage_apply_patches
        fi
    fi
}

pkg_patch_copy_files() {
    local IFS="$jagen_IFS" i=0 name value src dst dir
    while :; do
        i=$((i+1))
        name="\$pkg_files_$i"
        value=$(eval echo \"$name\")
        [ "$value" ] || break;
        set -- $value
        name=$1 src=$2 dst=${3:-$pkg_build_dir/$name} dir=$(dirname "$dst")
        [ -d "$dir" ] || pkg_run mkdir -p "$dir"
        pkg_run cp -va "$src" "$dst"
    done
}

pkg__generate_autogen() {
    message "generating GNU build system for $pkg_name using autogen.sh"
    pkg_run sh ./autogen.sh
}

pkg__generate_autoreconf() {
    message "generating GNU build system for $pkg_name using autoreconf"
    pkg_run autoreconf -vif
}

pkg_generate() {
    [ "$pkg_source_dir" ] || return 0
    case $pkg_build_generate in
        autogen|yes)
            if [ -f ./autogen.sh ]; then
                pkg__generate_autogen
            else
                pkg__generate_autoreconf
            fi ;;
        autoreconf)
            pkg__generate_autoreconf
            ;;
    esac
}
