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
        name="\$pkg_file_$i"
        value=$(eval echo \"$name\")
        [ "$value" ] || break;
        set -- $value
        name=$1 src=$2 dst=${3:-$pkg_build_dir/$name} dir=$(dirname "$dst")
        [ -d "$dir" ] || pkg_run mkdir -p "$dir"
        pkg_run cp -va "$src" "$dst"
    done
}
