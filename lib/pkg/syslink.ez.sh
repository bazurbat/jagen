#!/bin/sh

pworkdir="$ja_srcdir/syslink"
rules="$EZSDK/Rules.make"

platform=TI816X

MAKE="make DEVICE=$platform \
    GPPOS=Linux \
    LOADER=ELF \
    SDK=EZSDK \
    IPC_INSTALL_DIR=$EZSDK/component-sources/ipc_1_24_03_32 \
    BIOS_INSTALL_DIR=$EZSDK/component-sources/bios_6_33_05_46 \
    XDC_INSTALL_DIR=$EZSDK/component-sources/xdctools_3_23_03_53 \
    LINUXKERNEL=$ja_srcdir/linux \
    CGT_ARM_INSTALL_DIR=/opt/local/arm-2009q1/ \
    CGT_ARM_PREFIX=/opt/local/arm-2009q1/bin/arm-none-linux-gnueabi- \
    CGT_C674_ELF_INSTALL_DIR=$EZSDK/dsp-devkit/cgt6x_7_3_4 \
    USE_SYSLINK_NOTIFY=0"

pkg_unpack() {
    p_run $MAKE clean
}

pkg_build() {
    # do not use parallel jobs here
    p_run $MAKE syslink
}

pkg_install() {
    local src="$pworkdir/packages/ti/syslink"
    local dest="$rootfsdir/lib/modules/$kernel_version/kernel/drivers/dsp"

    p_run install -v -d "$dest"
    p_run install -v -m644 \
        "$src/bin/$platform/syslink.ko" "$dest"
    p_run install -v -m644 \
        $src/lib/syslink.a* "$rootfsdir/usr/lib"
    p_run install -v -m755 \
        $src/bin/$platform/samples/slaveloader* "$rootfsdir/usr/bin"
}
