#!/bin/sh

init() {
    pkg_run install -d -m 755 \
        bin   \
        dev   \
        etc   \
        home  \
        lib   \
        mnt   \
        proc  \
        root  \
        run   \
        sbin  \
        share \
        sys   \
        tmp   \
        usr   \
        var
}

copy_toolchain_libs() {
    local sysroot="$(${pkg_system}-gcc --print-sysroot)"

    pkg_run rsync -a "$sysroot/lib/" lib
    pkg_run rsync -lpt "$jagen_toolchain_dir"/arm-hisiv200-linux-gnueabi/lib/* lib
}

copy_sdk_files() {
    local pub_dir="$jagen_sdk_dir/pub"

    if [ -d "$pub_dir/rootfs" ]; then
        pkg_run rsync -rtl "$jagen_sdk_dir/pub/rootfs/" "."
    fi
    pkg_run rsync -rtl "$pub_dir/lib/share/" "lib"
    pkg_run rsync -rtl --delete "$pub_dir/kmod/" "kmod"
}

jagen_pkg_install() {
    local sysroot="$(${pkg_system}-gcc --print-sysroot)"

    init
    copy_toolchain_libs
    copy_sdk_files

    pkg_run rsync -t "$jagen_private_dir/lib/libHA.AUDIO.PCM.decode.so" "lib"

    pkg_run rsync -a "$pkg_install_dir/" .
    pkg_run rsync -aFF "$pkg_source_dir/hisi/" .

    echo "$jagen_target_board" > etc/hostname

    pkg_run ln -snf /proc/mounts etc/mtab
    pkg_run ln -snf /bin/busybox init
    pkg_run ln -snf /var settings

    pkg_run chmod 0700 root
    pkg_run chmod 0700 root/.ssh
    pkg_run chmod 1777 tmp

    pkg_run rm -rf include
    pkg_run find lib -type f '(' \
        -name '*.a' -o \
        -name '*.la' \
        ')' -delete
    pkg_run rm -rf lib/dbus* lib/libffi* lib/pkgconfig lib/xtables
    pkg_run rm -rf libexec/xtables-addons
    pkg_run find . -type f -name '.dirholder' -delete

    if [ -d var/service/connman ]; then
        pkg_run touch var/service/connman/down
    fi

    # if pkg_is_release; then
    #     _jagen src status > heads || return
    # fi
}
