#!/bin/sh

p_work_dir="$EZSDK/component-sources/linuxutils_3_22_00_02"

src_dir="$p_work_dir/packages/ti/sdo/linuxutils/cmem/src"
interface_dir="$src_dir/interface"
module_dir="$src_dir/module"

MAKE="make RULES_MAKE=$sdk_rules"

use_env target

pkg_clean() {
    p_run $MAKE -C $interface_dir clean
    p_run $MAKE -C $module_dir clean
}

pkg_unpack() { :; }

pkg_build() {
    p_run $MAKE -C $interface_dir ../../lib/cmem.a470MV
    p_run $MAKE -C $module_dir
}

pkg_install() {
    local dest="$kernel_modules_dir/kernel/drivers/dsp"

    p_run install -v -d "$dest"
    p_run install -v -m644 "$module_dir/cmemk.ko" "$dest"
}
