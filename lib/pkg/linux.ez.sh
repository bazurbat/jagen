#!/bin/sh

pworkdir="$ja_srcdir/$pname"

if [ "$target_board" = "ti_evm" ]; then
    defconfig="ti8168_evm_defconfig"
    uimage="uImage-evm"
elif [ "$target_board" = "ast200" ]; then
    defconfig="ast200_defconfig"
    uimage="uImage-ast200"
fi

pkg_build_target() {
    p_cmd $CROSS_MAKE $defconfig
    p_cmd $CROSS_MAKE uImage
    p_cmd $CROSS_MAKE modules
}

pkg_install_target() {
    local dest="$rootfsdir/boot"

    p_cmd install -d "$dest"
    p_cmd install -m644 "${pworkdir}/arch/arm/boot/uImage" "$dest/$uimage"
    p_cmd install -m644 "${pworkdir}/arch/arm/boot/uImage" "/tftproot/$uimage"
    p_cmd install -m644 "${pworkdir}/System.map" "$dest"

    p_cmd $CROSS_MAKE INSTALL_MOD_PATH="${dest}/" modules_install
}

pkg_depmod_target() {
    local depmod="/sbin/depmod"

    p_cmd $depmod -ae -F "$pworkdir/System.map" -b "$rootfsdir" "$kernel_version"
}
