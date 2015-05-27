#!/bin/sh

generate_toolchain_wrappers() {
    local bin name

    [ -d "$target_bin_dir" ] && rm -r "$target_bin_dir"
    mkdir -p "$target_bin_dir"

    if [ -d "$sdk_toolchain_dir" ]; then
        message "Generating HiSilicon SDK toolchain wrappers"

        for bin in "$sdk_toolchain_dir"/bin/*; do
            name="$(basename "$bin" | cut -d- -f5-)"
            ln -s "$bin" "${target_bin_dir}/${target_system}-${name}"
        done
    else
        if [ -z "$sdk_toolchain_dir" ]; then
            warning "sdk_toolchain_dir is not set"
        else
            warning "sdk_toolchain_dir '$sdk_toolchain_dir' is not exists"
        fi

        warning "Skip generating HiSilicon SDK toolchain wrappers"
    fi
}
