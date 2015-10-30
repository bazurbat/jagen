#!/bin/sh

p_jobs=1

use_env tools
use_toolchain target

export ARCH=mips
export KCFLAGS="-mhard-float -Wa,-mhard-float"

build_libs="rmcore rmcec rmedid rmhdmi rmhsi rmi2c rmsha1 rmvideoout rmoutput"

PATH="$SMP86XX_TOOLCHAIN_PATH/bin:$PATH"

pkg_build() {
    p_run make

    for lib in $build_libs; do
        p_run make -C "MRUA_src/$lib/src"
    done

    p_run make -C MRUA_src/gbuslib/src
    p_run make -C MRUA_src/rmoutput/rua/src
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
    local bin_dst="$jagen_install_dir/bin"
    local lib_dst="$jagen_install_dir/lib"

    for bin in ikc xkc; do
        p_run cp -va "$p_source_dir/bin/$bin" "$bin_dst"
    done

    for bin in gbus_read_bin_to_file gbus_read_uint32; do
        p_run cp -va "$p_source_dir/MRUA_src/llad_smallapps/$bin" "$bin_dst"
    done

    for bin in rmfree rmmalloc; do
        p_run cp -va "$p_source_dir/MRUA_src/llad_xtest/$bin" "$bin_dst"
    done

    for lib in $build_libs; do
        p_run cp -va "$p_source_dir/MRUA_src/$lib/src/lib${lib}.so" "$lib_dst"
    done

    p_run cp -va "$p_source_dir/MRUA_src/gbuslib/src/libgbus.so" "$lib_dst"
    p_run cp -va "$p_source_dir/MRUA_src/rmoutput/rua/src/libruaoutput.so" "$lib_dst"

    local libs="llad rmchannel rmcw rmmm rmmm_g rmmm_t rua"

    for lib in $libs; do
        p_run cp -va "$p_source_dir/lib/lib${lib}.so" "$lib_dst"
    done
}
