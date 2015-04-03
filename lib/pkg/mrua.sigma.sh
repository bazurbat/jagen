#!/bin/sh

p_source_dir="$pkg_src_dir/sigma-mrua"

with_rmdisplay="no"
with_rmoutput="yes"

case $pkg_sdk_version in
    308)
        p_source_branch="3.8.3"
        with_rmdisplay="yes"
        with_rmoutput="no"
        ;;
    309) p_source_branch="3.9.2"  ;;
    311) p_source_branch="3.11.3" ;;
    400) p_source_branch="4.0.0"  ;;
esac

p_jobs=1

use_env tools
use_toolchain target

export ARCH=mips
export KCFLAGS="-mhard-float -Wa,-mhard-float"

pkg_build() {
    p_run make
    p_run make
    p_run make -C MRUA_src/splashscreen/utils
}

pkg_modules() {
    local src_dir="$p_source_dir/modules/$kernel_release"

    p_run cd "$src_dir"
    p_install_modules em8xxx llad

    if in_flags with_alsa; then
        p_run cp -f \
            "$p_source_dir/MRUA_src/rua/emhwlib_kernel/kernel_src/em8xxxalsa.ko" \
            "$src_dir"
        p_install_modules em8xxxalsa
    fi
}

pkg_install() {
    local libs

    for bin in ikc xkc; do
        p_run install -vm 755 \
            "$p_source_dir/bin/$bin" \
            "$sdk_firmware_dir/bin"
    done

    for bin in gbus_read_bin_to_file gbus_read_uint32; do
        p_run install -vm 755 \
            "$p_source_dir/MRUA_src/llad_smallapps/$bin" \
            "$sdk_firmware_dir/bin"
    done

    for bin in rmfree rmmalloc; do
        p_run install -vm 755 \
            "$p_source_dir/MRUA_src/llad_xtest/$bin" \
            "$sdk_firmware_dir/bin"
    done

    libs="gbus llad rmchannel rmcore rmcw rmmm rmmm_g rmmm_t"

    libs="$libs \
        rmcec \
        rmedid \
        rmhdmi \
        rmhsi \
        rmi2c \
        rmsha1 \
        rmvideoout \
        rua"

    if [ "$with_rmdisplay" = "yes" ]; then
        libs="$libs \
            audiooutports \
            displayoutports \
            rmdisplay \
            ruahdmi \
            ruahsi \
            ruai2c"
    fi

    if [ "$with_rmoutput" = "yes" ]; then
        libs="$libs \
            rmoutput \
            ruaoutput"
    fi

    for lib in $libs; do
        p_run install -vm 755 \
            "$p_source_dir/lib/lib${lib}.so" \
            "$sdk_firmware_dir/lib"
    done

}
