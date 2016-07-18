#!/bin/sh

jagen_pkg_unpack() {
    toolchain_unpack "$pkg_name" "$pkg_source_dir"
}

jagen_pkg_install_target() {
    local name path src_name src_dir
    local target_system='arm-hisiv200-linux'

    src_dir="${pkg_source_dir:?}/bin"

    cd "${src_dir:?}" || return

    for src_name in *; do
        name=$(printf "$src_name" | cut -d- -f4-)
        path="${jagen_bin_dir:?}/${target_system}-${name}"
        cat >"$path" <<EOF || return
exec \$jagen_ccache "$src_dir/$src_name" "\$@"
EOF
        chmod +x "$path" || return
    done
}
