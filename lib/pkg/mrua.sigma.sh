#!/bin/sh

pworkdir="$ja_srcdir/sigma-mrua"

use_env tools target

pkg_build() {
    p_run make
    p_run make
    p_run make -C MRUA_src/splashscreen/utils
}

pkg_modules() {
    p_run cd "$pworkdir/modules/$kernelrelease"
    p_install_modules em8xxx llad
}

pkg_install() {
    p_run cd bin
    p_run cp -a ikc xkc "$firmwaredir/bin"

    p_run cd "$pworkdir/MRUA_src/llad_smallapps"
    p_run cp -a gbus_read_bin_to_file gbus_read_uint32 "$firmwaredir/bin"

    p_run cd "$pworkdir/MRUA_src/llad_xtest"
    p_run cp -a rmfree rmmalloc "$firmwaredir/bin"

    p_run cd "$pworkdir/lib"
    p_run cp -a \
        libgbus.so \
        libllad.so \
        librmchannel.so \
        librmcore.so \
        librmcw.so \
        librmmm.so \
        librmmm_g.so \
        librmmm_t.so \
        "$firmwaredir/lib"

    p_run cd "$pworkdir/lib"
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
        "$firmwaredir/lib"

    if [ "$sdkver" = "4.0" ]; then
        p_run cp -a librminfoframe.so "$firmwaredir/lib"
    fi
}
