#!/bin/sh

toolchain_get_sysroot() {
    real_path $("${CC:?}" --print-sysroot)
}

toolchain_get_arch_sysroot() {
    real_path $("${CC:?}" $ARCH_CFLAGS --print-sysroot)
}

toolchain_get_arch_subdir() {
    local sysroot="$(toolchain_get_sysroot)"
    local arch_sysroot="$(toolchain_get_arch_sysroot)"
    printf "$arch_sysroot" | sed -re "s|$sysroot/||"
}

toolchain_get_lib_dir() {
    local libstdc="$("$CC" $ARCH_CFLAGS --print-file-name=libstdc++.a)"
    real_path $(dirname "$libstdc")
}
