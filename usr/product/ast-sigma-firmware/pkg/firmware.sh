#!/bin/sh

create_imaterial() {
    local workdir="$jagen_target_dir/imaterial"
    local bmp2sdd="${mrua_dir:?}/MRUA_src/splashscreen/utils/bmp2sdd"

    rm -rf "$workdir" && mkdir -p "$workdir" || return $?

    pkg_run cp -f \
        "$jagen_private_dir/ucode/itask_loader.iload" \
        "$jagen_private_dir/ucode/itask_splashscreen.iload" \
        "$workdir"

    pkg_run "$bmp2sdd" \
        "$jagen_private_dir/splash/artsystem-splash-2013-720p-32bpp.bmp" \
        "$workdir/splash_picture.sdd"

    pkg_run genromfs -d "$workdir" -f "$jagen_target_dir/imaterial.romfs" -V imaterial
}

create_xmaterial() {
    local workdir="$jagen_target_dir/xmaterial"

    rm -rf "$workdir" && mkdir -p "$workdir" || return $?

    pkg_run cp -f \
        "$jagen_private_dir/ucode/xtask_loader.xload" \
        "$jagen_private_dir/ucode/ios.bin.gz_8644_ES1_dev_0006.xload" \
        "$workdir"

    pkg_run genromfs -d "$workdir" -f "$jagen_target_dir/xmaterial.romfs" -V xmaterial
}

jagen_pkg_compile() {
    create_imaterial || return $?
    create_xmaterial || return $?
}

install_dbus() {
    pkg_run cp -vaf "$pkg_source_dir/etc/dbus-1" "$pkg_build_dir/etc"
    pkg_run install -vm755 "$pkg_source_dir"/bin/dbus-* "$pkg_build_dir/bin"
    pkg_run install -vm755 \
        "$pkg_source_dir/libexec/dbus-daemon-launch-helper" \
        "$pkg_build_dir/libexec"
}

install_rsync() {
    pkg_run install -vm755 "$pkg_source_dir/bin/rsync" "$pkg_build_dir/bin"
}

install_wpa_supplicant() {
    pkg_run install -m755 \
        "$pkg_source_dir/bin/wpa_cli" \
        "$pkg_source_dir/bin/wpa_passphrase" \
        "$pkg_build_dir/bin"
    pkg_run install -m755 \
        "$pkg_source_dir/sbin/wpa_supplicant" \
        "$pkg_build_dir/sbin"
}

install_mrua() {
    local bin="ikc xkc gbus_read_bin_to_file gbus_read_uint32 rmfree rmmalloc"

    ( cd "$pkg_install_dir/bin"
      pkg_run install -vm755 $bin "$pkg_build_dir/bin"
    )
}

install_ucode() {
    local src_dir="$pkg_install_dir/xload"
    local dst_dir="$pkg_build_dir/xload"
    pkg_run mkdir -p "$dst_dir"
    pkg_run cp -va "$src_dir"/* "$dst_dir"
}

do_cleanup() {
    pkg_run cd "$pkg_build_dir"

    pkg_run find lib -type f \
        "(" -name "*.a" -o -name "*.la" ")" \
        -print -delete

    pkg_run find lib/chicken -type f "(" \
        -name "*.o" \
        -o -name "*.setup-info" \
        -o -name "*.inline" \
        -o -name "*.types" \
        ")" -print -delete

    if pkg_is_release; then
        pkg_run find lib/chicken -type f "(" \
            -name "*.import.*" \
            -o -name "*.scm" \
            -o -name "types.db" \
            ")" -print -delete

        pkg_strip_root "$pkg_build_dir"
    fi
}

jagen_pkg_install() {
    local bin="audioplayer demo jabba midiplayer smplayer db-service \
        csi i2c_debug uart-shell ast-service pcf8563 agent-smith"

    pkg_run install -d -m 755 bin dev etc home lib libexec mnt proc run sbin share sys usr var
    pkg_run install -d -m 700 root
    pkg_run install -d -m 1777 tmp

    pkg_run cd "$pkg_source_dir/bin"
    pkg_run install -vm755 $bin "$pkg_build_dir/bin"

    pkg_run install -vm755 \
        "$pkg_source_dir/sbin/connmand" \
        "$pkg_build_dir/sbin"

    pkg_run cd "$pkg_source_dir/lib"
    pkg_run cp -va chicken "$pkg_build_dir/lib"
    pkg_run cp -va *.so* "$pkg_build_dir/lib"

    pkg_run cp -af "$pkg_source_dir/etc" "$pkg_build_dir"
    pkg_run cp -af "$pkg_source_dir/share/dbus-1" "$pkg_build_dir/share"

    pkg_run cp -vf "$jagen_target_dir/xmaterial.romfs" "$pkg_build_dir"
    pkg_run cp -vf "$jagen_target_dir/imaterial.romfs" "$pkg_build_dir"
    pkg_run cp -vf "$jagen_target_dir/zbimage-linux-xload.zbc" "$pkg_build_dir"
    pkg_run cp -vf "$jagen_target_dir/phyblock0-0x20000padded.AST50" "$pkg_build_dir"
    pkg_run cp -vf "$jagen_target_dir/phyblock0-0x20000padded.AST100" "$pkg_build_dir"

    install_dbus
    install_rsync
    install_wpa_supplicant
    install_mrua || return
    install_ucode

    # delete not used libraries
    pkg_run rm -f "$pkg_build_dir"/lib/libxt_*
    pkg_run rm -f \
        "$pkg_build_dir"/lib/libgio* \
        "$pkg_build_dir"/lib/libgmodule* \
        "$pkg_build_dir"/lib/libgobject*
    pkg_run rm -f \
        "$pkg_build_dir"/lib/libnl-idiag* \
        "$pkg_build_dir"/lib/libnl-nf* \
        "$pkg_build_dir"/lib/libnl-route*

    pkg_run cp -vaf "$jagen_private_dir"/firmware/* "$pkg_build_dir"

    do_cleanup

    _jagen src status > "$pkg_build_dir/heads" || die
}
