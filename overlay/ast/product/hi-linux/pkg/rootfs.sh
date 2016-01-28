#!/bin/sh

jagen_pkg_install() {
    local src="$pkg_build_dir"
    local dst="$pkg_source_dir/usr"

    local programs="csi smplayer"

    for name in $programs; do
        pkg_run install -m 755 "$src/bin/$name" "$dst/bin"
    done

    pkg_run rsync -rtlm --include='*/' --include='*.so*' --exclude='*' \
        "$src/lib" "$dst"
    pkg_run rsync -rtl "$src/lib/chicken" "$dst/lib"
    find "$dst/lib" -name '*.so*' -exec chmod 755 '{}' \+ || die
}
