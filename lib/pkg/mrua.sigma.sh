#!/bin/sh

p_source_dir="$pkg_src_dir/sigma-mrua"

case $pkg_sdk_version in
    308) p_source_branch="3.8.3"  ;;
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
    p_run cd bin
    p_run cp -a ikc xkc "$sdk_firmware_dir/bin"

    p_run cd "$p_source_dir/MRUA_src/llad_smallapps"
    p_run cp -a gbus_read_bin_to_file gbus_read_uint32 "$sdk_firmware_dir/bin"

    p_run cd "$p_source_dir/MRUA_src/llad_xtest"
    p_run cp -a rmfree rmmalloc "$sdk_firmware_dir/bin"

    p_run cd "$p_source_dir/lib"
    p_run cp -a \
        libgbus.so \
        libllad.so \
        librmchannel.so \
        librmcore.so \
        librmcw.so \
        librmmm.so \
        librmmm_g.so \
        librmmm_t.so \
        "$sdk_firmware_dir/lib"

    p_run cd "$p_source_dir/lib"
    p_run cp -a \
        librmcec.so \
        librmedid.so \
        librmhdmi.so \
        librmhsi.so \
        librmi2c.so \
        librmsha1.so \
        librmvideoout.so \
        librua.so \
        "$sdk_firmware_dir/lib"

    case $pkg_sdk_version in
        308)
            p_run cp -a \
                librmdisplay.so \
                "$sdk_firmware_dir/lib"
            ;;
        *)
            p_run cp -a \
                librmoutput.so \
                libruaoutput.so \
                "$sdk_firmware_dir/lib"
            ;;
    esac
}
