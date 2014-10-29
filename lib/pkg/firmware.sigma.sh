#!/bin/sh

use_env target

pworkdir="$firmwaredir"
psourcedir="${targetdir}${targetprefix}"

pkg_unpack() {
    p_clean "$pworkdir"
    p_clean "$psourcedir"

    p_run cd "$pworkdir"

    p_run install -d -m 755 bin dev etc home lib mnt proc run sbin sys usr var
    p_run install -d -m 700 root
    p_run install -d -m 1777 tmp
}

create_imaterial() {
    local workdir="$targetdir/imaterial"
    local bmp2sdd="$sdk_mrua_dir/MRUA_src/splashscreen/utils/bmp2sdd"

    rm -rf "$workdir" && mkdir -p "$workdir" || return $?

    p_run cp -f \
        "$sdk_files_dir/ucode/itask_loader.iload" \
        "$sdk_files_dir/ucode/itask_splashscreen.iload" \
        "$workdir"

    p_run "$bmp2sdd" \
        "$sdk_files_dir/splash/artsystem-splash-2013-720p-32bpp.bmp" \
        "$workdir/splash_picture.sdd"

    p_run genromfs -d "$workdir" -f "$targetdir/imaterial.romfs" -V imaterial
}

create_xmaterial() {
    local workdir="$targetdir/xmaterial"

    rm -rf "$workdir" && mkdir -p "$workdir" || return $?

    p_run cp -f \
        "$sdk_files_dir/ucode/xtask_loader.xload" \
        "$sdk_files_dir/ucode/ios.bin.gz_8644_ES1_dev_0006.xload" \
        "$workdir"

    p_run genromfs -d "$workdir" -f "$targetdir/xmaterial.romfs" -V xmaterial
}

pkg_material() {
    create_imaterial || return $?
    create_xmaterial || return $?
}

copy_files() {
    cp -af "$sdk_files_dir"/firmware/* "$firmwaredir" || return $?
}

install_chibi() {
    local src="$psourcedir"
    local dst="$firmwaredir"

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

    cd "$psourcedir/bin" || return $?
    install -m 755 $bin "$firmwaredir/bin" || return $?

    cd "$psourcedir/lib" || return $?
    cp -a chicken "$firmwaredir/lib" || return $?
    cp -a *.so* "$firmwaredir/lib" || return $?

    copy_files || return $?

    # install_chibi || return $?

    cp -f "$targetdir/xmaterial.romfs" "$firmwaredir/" || return $?
    cp -f "$targetdir/imaterial.romfs" "$firmwaredir/" || return $?
    cp -f "$targetdir/zbimage-linux-xload.zbc" "$firmwaredir/" || return $?
    cp -f "$targetdir/phyblock0-0x20000padded.AST50" "$firmwaredir/" || return $?
    cp -f "$targetdir/phyblock0-0x20000padded.AST100" "$firmwaredir/" || return $?

    cd "$sdk_rootfs_prefix/lib" || return $?
    cp -a libsqlite* "$firmwaredir/lib" || return $?
}

pkg_clean() {
    cd "$pworkdir" || return $?

    find lib usr/lib -type f "(" \
        -name "*.a" -o \
        -name "*.la" \
        ")" -print -delete \
        >"$plog" 2>&1

    find lib/chicken -type f "(" \
        -name "*.o" -o \
        -name "*.setup-info" -o \
        -name "*.types" -o \
        -name "*.inline" -o \
        ")" -print -delete \
        >>"$plog" 2>&1

    if [ "$ja_buildtype" = "Release" ]; then
        find lib/chicken -type f "(" \
            -name "*.import.so" -o \
            -name "*.scm" -o \
            -name "types.db" -o \
            ")" -print -delete \
            >>"$plog" 2>&1

        p_strip "$pworkdir" >>"$plog" 2>&1
    fi
}
