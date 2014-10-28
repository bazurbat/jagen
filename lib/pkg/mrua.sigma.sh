#!/bin/sh

pworkdir="$ja_srcdir/sigma-mrua"

use_env tools target

pkg_build() {
    p_make
    p_make
    p_make -C MRUA_src/splashscreen/utils
}

pkg_modules() {
    p_cmd cd "$pworkdir/modules/$kernelrelease"
    p_install_modules em8xxx llad
}

pkg_install() {
    p_cmd cd bin
    p_cmd cp -a ikc xkc "$firmwaredir/bin"

    p_cmd cd "$pworkdir/MRUA_src/llad_smallapps"
    p_cmd cp -a gbus_read_bin_to_file gbus_read_uint32 "$firmwaredir/bin"

    p_cmd cd "$pworkdir/MRUA_src/llad_xtest"
    p_cmd cp -a rmfree rmmalloc "$firmwaredir/bin"

    p_cmd cd "$pworkdir/lib"
    p_cmd cp -a \
        libgbus.so \
        libllad.so \
        librmchannel.so \
        librmcore.so \
        librmcw.so \
        librmmm.so \
        librmmm_g.so \
        librmmm_t.so \
        "$firmwaredir/lib"

    p_cmd cd "$pworkdir/lib"
    p_cmd cp -a \
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
        p_cmd cp -a librminfoframe.so "$firmwaredir/lib"
    fi
}
