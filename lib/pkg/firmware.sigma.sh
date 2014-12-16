#!/bin/sh

use_toolchain target

p_work_dir="$sdk_firmware_dir"
p_source_dir="${target_dir}${target_prefix}"

pkg_clean() {
    p_clean_dir "$p_work_dir"
    p_clean_dir "$p_source_dir"
}

pkg_unpack() {
    p_run cd "$p_work_dir"

    p_run install -d -m 755 bin dev etc home lib libexec mnt proc run sbin sys usr var
    p_run install -d -m 755 usr/bin usr/lib usr/sbin
    p_run install -d -m 700 root
    p_run install -d -m 1777 tmp
}

create_imaterial() {
    local workdir="$target_dir/imaterial"
    local bmp2sdd="$sdk_mrua_dir/MRUA_src/splashscreen/utils/bmp2sdd"

    rm -rf "$workdir" && mkdir -p "$workdir" || return $?

    p_run cp -f \
        "$pkg_private_dir/ucode/itask_loader.iload" \
        "$pkg_private_dir/ucode/itask_splashscreen.iload" \
        "$workdir"

    p_run "$bmp2sdd" \
        "$pkg_private_dir/splash/artsystem-splash-2013-720p-32bpp.bmp" \
        "$workdir/splash_picture.sdd"

    p_run genromfs -d "$workdir" -f "$target_dir/imaterial.romfs" -V imaterial
}

create_xmaterial() {
    local workdir="$target_dir/xmaterial"

    rm -rf "$workdir" && mkdir -p "$workdir" || return $?

    p_run cp -f \
        "$pkg_private_dir/ucode/xtask_loader.xload" \
        "$pkg_private_dir/ucode/ios.bin.gz_8644_ES1_dev_0006.xload" \
        "$workdir"

    p_run genromfs -d "$workdir" -f "$target_dir/xmaterial.romfs" -V xmaterial
}

pkg_material() {
    create_imaterial || return $?
    create_xmaterial || return $?
}

install_dbus() {
    p_run cp -vaf "$p_source_dir/etc/dbus-1" "$p_work_dir/etc"
    p_run install -vm755 "$p_source_dir"/bin/dbus-* "$p_work_dir/bin"
    p_run install -vm755 \
        "$p_source_dir/libexec/dbus-daemon-launch-helper" \
        "$p_work_dir/libexec"
}

install_rsync() {
    p_run install -vm755 "$p_source_dir/bin/rsync" "$p_work_dir/bin"
}

install_wpa_supplicant() {
    p_run install -m755 \
        "$p_source_dir/bin/wpa_cli" \
        "$p_source_dir/bin/wpa_passphrase" \
        "$p_work_dir/bin"
    p_run install -m755 \
        "$p_source_dir/sbin/wpa_supplicant" \
        "$p_work_dir/sbin"
}

pkg_install() {
    local bin="audioplayer bgaudio demo jabba midiplayer smplayer db-service \
        csi i2c_debug uart-shell ast-service pcf8563 agent-smith"

    p_run cd "$p_source_dir/bin"
    p_run install -vm 755 $bin "$sdk_firmware_dir/bin"

    p_run install -vm 755 \
        "$p_source_dir/sbin/connmand" \
        "$sdk_firmware_dir/sbin"

    p_run cd "$p_source_dir/lib"
    p_run cp -va chicken "$sdk_firmware_dir/lib"
    p_run cp -va *.so* "$sdk_firmware_dir/lib"

    p_run cp -af "$p_source_dir/etc" "$sdk_firmware_dir"
    p_run cp -af "$p_source_dir/share/dbus-1" "$sdk_firmware_dir/share"

    p_run cp -vf "$target_dir/xmaterial.romfs" "$sdk_firmware_dir/"
    p_run cp -vf "$target_dir/imaterial.romfs" "$sdk_firmware_dir/"
    p_run cp -vf "$target_dir/zbimage-linux-xload.zbc" "$sdk_firmware_dir/"
    p_run cp -vf "$target_dir/phyblock0-0x20000padded.AST50" "$sdk_firmware_dir/"
    p_run cp -vf "$target_dir/phyblock0-0x20000padded.AST100" "$sdk_firmware_dir/"

    install_dbus
    install_rsync
    install_wpa_supplicant

    # delete not used libraries
    p_run rm -f "$p_work_dir"/lib/libxt_*
    p_run rm -f \
        "$p_work_dir"/lib/libgio* \
        "$p_work_dir"/lib/libgmodule* \
        "$p_work_dir"/lib/libgobject*

    p_run cp -vaf "$pkg_private_dir"/firmware/* "$sdk_firmware_dir"
}

pkg_strip() {
    p_run cd "$p_work_dir"

    p_run find lib usr/lib -type f \
        "(" -name "*.a" -o -name "*.la" ")" \
        -print -delete

    p_run find lib/chicken -type f "(" \
        -name "*.o" \
        -o -name "*.setup-info" \
        -o -name "*.inline" \
        -o -name "*.types" \
        ")" -print -delete

    if [ "$pkg_build_type" = "Release" ]; then
        p_run find lib/chicken -type f "(" \
            -name "*.import.*" \
            -o -name "*.scm" \
            -o -name "types.db" \
            ")" -print -delete

        p_strip "$p_work_dir"
    fi
}
