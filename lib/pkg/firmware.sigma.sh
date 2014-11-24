#!/bin/sh

use_env target

p_work_dir="$sdk_firmware_dir"
p_source_dir="${target_dir}${target_prefix}"

pkg_clean() {
    p_clean_dir "$p_work_dir"
    p_clean_dir "$p_source_dir"
}

pkg_unpack() {
    p_run cd "$p_work_dir"

    p_run install -d -m 755 bin dev etc home lib mnt proc run sbin sys usr var
    p_run install -d -m 755 usr/bin usr/lib usr/sbin
    p_run install -d -m 700 root
    p_run install -d -m 1777 tmp
}

create_imaterial() {
    local workdir="$target_dir/imaterial"
    local bmp2sdd="$sdk_mrua_dir/MRUA_src/splashscreen/utils/bmp2sdd"

    rm -rf "$workdir" && mkdir -p "$workdir" || return $?

    p_run cp -f \
        "$ja_files_dir/ucode/itask_loader.iload" \
        "$ja_files_dir/ucode/itask_splashscreen.iload" \
        "$workdir"

    p_run "$bmp2sdd" \
        "$ja_files_dir/splash/artsystem-splash-2013-720p-32bpp.bmp" \
        "$workdir/splash_picture.sdd"

    p_run genromfs -d "$workdir" -f "$target_dir/imaterial.romfs" -V imaterial
}

create_xmaterial() {
    local workdir="$target_dir/xmaterial"

    rm -rf "$workdir" && mkdir -p "$workdir" || return $?

    p_run cp -f \
        "$ja_files_dir/ucode/xtask_loader.xload" \
        "$ja_files_dir/ucode/ios.bin.gz_8644_ES1_dev_0006.xload" \
        "$workdir"

    p_run genromfs -d "$workdir" -f "$target_dir/xmaterial.romfs" -V xmaterial
}

pkg_material() {
    create_imaterial || return $?
    create_xmaterial || return $?
}

install_chibi() {
    local src="$p_source_dir"
    local dst="$sdk_firmware_dir"

    cd "$src" || return $?
    cp -a bin/chibi-scheme "$dst/bin" || return $?
    cp -a lib/*chibi* "$dst/lib" || return $?
    mkdir -p "$dst/share"
    cp -a share/chibi "$dst/share" || return $?

    cd "$dst/lib"
    ln -sf "libchibi-scheme.so.0.7" "libchibi-scheme.so"
    ln -sf "libchibi-scheme.so.0.7" "libchibi-scheme.so.0"
}

pkg_install() {
    local bin="audioplayer bgaudio demo jabba midiplayer smplayer db-service \
        csi i2c_debug uart-shell ast-service pcf8563"

    p_run cd "$p_source_dir/bin"
    p_run install -vm 755 $bin "$sdk_firmware_dir/bin"

    p_run cd "$p_source_dir/lib"
    p_run cp -va chicken "$sdk_firmware_dir/lib"
    p_run cp -va *.so* "$sdk_firmware_dir/lib"

    p_run cp -af "$ja_files_dir"/firmware/* "$sdk_firmware_dir"

    # install_chibi || return $?

    p_run cp -vf "$target_dir/xmaterial.romfs" "$sdk_firmware_dir/"
    p_run cp -vf "$target_dir/imaterial.romfs" "$sdk_firmware_dir/"
    p_run cp -vf "$target_dir/zbimage-linux-xload.zbc" "$sdk_firmware_dir/"
    p_run cp -vf "$target_dir/phyblock0-0x20000padded.AST50" "$sdk_firmware_dir/"
    p_run cp -vf "$target_dir/phyblock0-0x20000padded.AST100" "$sdk_firmware_dir/"

    p_run cd "$sdk_rootfs_prefix/lib" || return $?
    p_run cp -va libsqlite* "$sdk_firmware_dir/lib" || return $?
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

        p_strip "$p_work_dir" >>"$p_log" 2>&1
    fi
}
