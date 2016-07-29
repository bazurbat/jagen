#!/bin/sh

generate_list() {
    cat >"${KERNEL_SRC:?}/usr/initramfs_list" <<EOF
dir /dev  0755 0 0
dir /etc  0755 0 0
dir /mnt  0755 0 0
dir /proc 0755 0 0
dir /run  0755 0 0
dir /sys  0755 0 0
dir /tmp  1777 0 0
dir /var  0755 0 0
nod /dev/console 0600 0 0 c 5 1
nod /dev/null    0666 0 0 c 1 3
EOF
}

jagen_pkg_configure() {
    generate_list
}

install_bin() {
    pkg_run cd "$pkg_install_dir/bin"
    pkg_run rsync -tlp --chmod=755 \
        $(eval echo "$@") "$pkg_build_dir/bin/"
    pkg_run cd "$OLDPWD"
}

install_sbin() {
    pkg_run cd "$pkg_install_dir/sbin"
    pkg_run rsync -tlp --chmod=755 \
        $(eval echo "$@") "$pkg_build_dir/sbin/"
    pkg_run cd "$OLDPWD"
}

install_lib() {
    pkg_run cd "$pkg_install_dir/lib"
    pkg_run rsync -tlp --chmod=755 \
        $(eval echo "$@") "$pkg_build_dir/lib/"
    pkg_run cd "$OLDPWD"
}

install_module() {
    : ${jagen_kernel_version:?}

    pkg_run cd "$pkg_install_dir/lib/modules/$jagen_kernel_version"
    pkg_run rsync -tlp --chmod=644 \
        $(eval echo "$@") "$pkg_build_dir/lib/modules/"
    pkg_run cd "$OLDOWD"
}

generate_init() {
    cat >"$pkg_build_dir/init" <<EOF || return
#!/bin/sh
mount -t devtmpfs devtmpfs /dev
mount -t proc proc /proc
mount -t sysfs sysfs /sys
insmod /lib/modules/loop.ko
exec /bin/sh
EOF
    chmod 755 "$pkg_build_dir/init"
}

jagen_pkg_install() {
    : ${pkg_install_dir:?}
    : ${pkg_build_dir:?}

    install_bin \
        busybox \
        mount \
        printf \
        sh

    install_sbin \
        insmod \
        losetup \
        switch_root 

    install_lib 'ld-*' \
        'libcrypt-*' 'libcrypt.*' \
        'libc-*' 'libc.*' \
        'libm-*' 'libm.*'

    install_module "extra/loop.ko"

    generate_init
}
