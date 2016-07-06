#!/bin/sh

jagen_pkg_install_target() {
    : ${jagen_toolchain_dir:?}

    local bin name path

    cd "$jagen_toolchain_dir/bin" || return

    for bin in *; do
        name=$(basename "$bin" | cut -d- -f5-)
        path="${jagen_bin_dir}/${jagen_target_system}-${name}"
        cat >"$path" <<EOF || return
exec \$jagen_ccache "\$jagen_toolchain_dir/bin/$bin" "\$@"
EOF
        chmod +x "$path" || return
    done

    toolchain_copy_sysroot
}
