#!/bin/sh

p_source="git git@bitbucket.org:art-system/sigma-mrua.git"
p_source_dir="$pkg_src_dir/sigma-mrua"
p_jobs=1

use_env tools
use_toolchain target

pkg_build() {
    p_run make
    p_run make
    p_run make -C MRUA_src/splashscreen/utils
}

pkg_modules() {
    p_run cd "$p_source_dir/modules/$kernel_release"
    p_install_modules em8xxx llad
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
        librmoutput.so \
        librmsha1.so \
        librmvideoout.so \
        librua.so \
        libruaoutput.so \
        "$sdk_firmware_dir/lib"
}
