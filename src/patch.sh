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
