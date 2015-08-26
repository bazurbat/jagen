#!/bin/sh

export AR="${toolchain_bin_dir}/${target_system}-ar"
export CC="${toolchain_bin_dir}/${target_system}-gcc"
export CXX="${toolchain_bin_dir}/${target_system}-g++"
export STRIP="${toolchain_bin_dir}/${target_system}-strip"

export CFLAGS=""
export CXXFLAGS="$CFLAGS"
export ASMFLAGS="$CFLAGS"
export LDFLAGS=""

export PKG_CONFIG_SYSROOT_DIR="$target_dir"
export PKG_CONFIG_LIBDIR="$target_dir$target_prefix/lib/pkgconfig"

add_PATH "$toolchain_bin_dir"

make_toolchain() {
    local bin name

    if ! [ "$jagen_toolchain_dir" ]; then
        error "jagen_toolchain_dir is not set"
        return 1
    fi

    [ -d "$toolchain_bin_dir" ] && rm -r "$toolchain_bin_dir"
    mkdir -p "$toolchain_bin_dir"

    for bin in "$jagen_toolchain_dir"/bin/*; do
        name="$(basename "$bin" | cut -d- -f5-)"
        ln -sf "$bin" "${toolchain_bin_dir}/${target_system}-${name}"
    done
}
