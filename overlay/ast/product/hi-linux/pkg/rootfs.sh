#!/bin/sh

install_key() {
    local src="$jagen_private_dir/rootfs"
    local dst="$pkg_source_dir"

    pkg_run mkdir -p "$dst/etc/dropbear"
    pkg_run cp -fv "$src/etc/dropbear/"* "$dst/etc/dropbear"

    pkg_run cp -frv "$src/root/.ssh" "$dst/root/.ssh"
    pkg_run chmod 700 "$dst/root/.ssh"
}

jagen_pkg_install() {
    local src="$pkg_build_dir"
    local dst="$pkg_source_dir/usr"

    local programs="csi smplayer dropbearkey"

    for name in $programs; do
        pkg_run install -m 755 "$src/bin/$name" "$dst/bin"
    done

    local sbin="dropbear"

    for name in $sbin; do
        pkg_run install -m 755 "$src/sbin/$name" "$dst/sbin"
    done

    pkg_run rsync -vrtlm --include='*/' --include='*.so*' --exclude='*' \
        "$src/lib" "$dst"
    pkg_run rsync -vrtl "$src/lib/chicken" "$dst/lib"
    find "$dst/lib" -name '*.so*' -exec chmod 755 '{}' \+ || die

    install_key || return

    cat >"$jagen_sdk_dir/pub/rootfs/etc/init.d/S99init" <<EOF
#!/bin/sh
udhcpc -i eth0
dropbear
EOF
    pkg_run chmod 755 "$jagen_sdk_dir/pub/rootfs/etc/init.d/S99init"
}
