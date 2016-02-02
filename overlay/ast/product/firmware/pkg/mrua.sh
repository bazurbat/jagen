#!/bin/sh

pkg_run_jobs=1

use_env tools
use_env target

export ARCH=mips
export KCFLAGS="-mhard-float -Wa,-mhard-float"

build_libs="rmcore rmcec rmedid rmhdmi rmhsi rmi2c rmsha1 rmvideoout rmoutput"

PATH="$SMP86XX_TOOLCHAIN_PATH/bin:$PATH"

jagen_pkg_compile() {
    pkg_run make

    for lib in $build_libs; do
        pkg_run make -C "MRUA_src/$lib/src"
    done

    pkg_run make -C MRUA_src/gbuslib/src
    pkg_run make -C MRUA_src/rmoutput/rua/src
    pkg_run make -C MRUA_src/splashscreen/utils
}

jagen_pkg_modules() {
    local src_dir="$pkg_source_dir/modules/$jagen_kernel_release"

    pkg_run cd "$src_dir"
    pkg_install_modules em8xxx llad
}

jagen_pkg_install() {
    local bin_dir="$jagen_target_dir$jagen_target_prefix/bin"
    local lib_dir="$jagen_target_dir$jagen_target_prefix/lib"

    pkg_run mkdir -p "$bin_dir" "$lib_dir"

    for bin in ikc xkc; do
        pkg_run cp -va "$pkg_source_dir/bin/$bin" "$bin_dir"
    done

    for bin in gbus_read_bin_to_file gbus_read_uint32; do
        pkg_run cp -va "$pkg_source_dir/MRUA_src/llad_smallapps/$bin" \
            "$bin_dir"
    done

    for bin in rmfree rmmalloc; do
        pkg_run cp -va "$pkg_source_dir/MRUA_src/llad_xtest/$bin" "$bin_dir"
    done

    for lib in $build_libs; do
        pkg_run cp -va "$pkg_source_dir/MRUA_src/$lib/src/lib${lib}.so" \
            "$lib_dir"
    done

    pkg_run cp -va \
        "$pkg_source_dir/MRUA_src/gbuslib/src/libgbus.so" \
        "$pkg_source_dir/MRUA_src/rmoutput/rua/src/libruaoutput.so" \
        "$lib_dir"

    local libs="llad rmchannel rmcw rmmm rmmm_g rmmm_t rua"

    for lib in $libs; do
        pkg_run cp -va "$pkg_source_dir/lib/lib${lib}.so" "$lib_dir"
    done
}
