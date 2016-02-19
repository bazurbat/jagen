#!/bin/sh

jagen_pkg_install_target() {
    : ${jagen_toolchain_bin_dir:?}
    : ${jagen_toolchain_dir:?}
    : ${jagen_toolchain_prefix:?}

    local bin name path

    rm -fr "$jagen_toolchain_bin_dir" || return
    mkdir -p "$jagen_toolchain_bin_dir" || return

    cd "$jagen_toolchain_dir/bin" || return

    for bin in *; do
        name=$(basename "$bin" | cut -d- -f5-)
        path="${jagen_toolchain_prefix}${name}"
        cat >"$path" <<EOF || return
exec \$jagen_ccache "\$jagen_toolchain_dir/bin/$bin" "\$@"
EOF
        chmod +x "$path" || return
    done
}
