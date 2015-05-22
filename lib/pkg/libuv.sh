#!/bin/sh

if [ "$pkg_sdk" = sigma ]; then
    p_build_dir="$p_work_dir/build${p_config:+-$p_config}"
fi

pkg_patch() {
    if [ "$pkg_sdk" = sigma ]; then
        p_run cd "$p_source_dir"
        p_run ./autogen.sh
    else
        git clone https://chromium.googlesource.com/external/gyp.git build/gyp
    fi
}

pkg_build_host() {
    p_run "$p_source_dir/configure" \
        --prefix="$host_prefix"

    p_run make
}

pkg_install_host() {
    p_run make DESTDIR="$host_dir" install
    p_fix_la "$host_dir$host_prefix/lib/libuv.la" "$host_dir"
}

pkg_build_target() {
    if [ "$pkg_sdk" = "sigma" ]; then
        p_run "$p_source_dir/configure" \
            --host="$target_system" \
            --prefix="$target_prefix"
        p_run make
    elif [ "$pkg_sdk" = hisilicon ]; then
        export AR=arm-linux-androideabi-ar
        export CC=arm-linux-androideabi-gcc
        export CXX=arm-linux-androideabi-g++
        export LINK=arm-linux-androideabi-g++
        export PLATFORM=android

        "$p_source_dir/gyp_uv.py" -Dtarget_arch=${target_arch} -DOS=android
        BUILDTYPE="$pkg_build_type" p_run make -C "$p_source_dir/out"
    fi
}

pkg_install_target() {
    out_dir="$p_source_dir/out"

    if [ "$pkg_sdk" = sigma ]; then
        p_run make DESTDIR="$target_dir" install
        p_fix_la "$target_dir$target_prefix/lib/libuv.la" "$target_dir"
    else
        p_run install -vm 644 "$out_dir/$pkg_build_type/libuv.a" \
            "${target_dir}${target_prefix}/lib"
    fi
}
