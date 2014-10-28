#!/bin/sh

if [ -z "$ja_basedir" ]; then
    export ja_basedir="$(realpath $(dirname $0)/..)"
fi

. "$ja_basedir/lib/jagen/env.sh" || exit

p_is_function() {
    type "$1" 2>/dev/null | grep -q 'function'
}

p_run() {
    debug "$*"
    "$@" >>"$plog" 2>&1 || exit
}

p_clean() {
    if [ -d "$1" ]; then
        p_run rm -fr "$1"
    fi
    p_run mkdir -p "$1"
}

p_strip() {
    local root files
    root="$1"
    files=$(find "$root" -type f -not -name "*.ko" \
        "(" -path "*/lib*" -o -path "*/bin*" -o -path "*/sbin*" ")" | \
        xargs -r file | grep "ELF.*\(executable\|shared object\).*not stripped" | cut -d: -f1)

    for f in $files; do
        "$STRIP" -v --strip-unneeded \
            -R .comment \
            -R .GCC.command.line \
            -R .note.gnu.gold-version \
            "$f"
    done
}

p_unpack() {
    [ "$1" ] || die "No source"
    local A="$(ls $distdir/${1}*)"
    tar -C "$pworkdir" -xf $A
}

p_patch() {
    p_run patch -sp1 -i "$distdir/patches/${1}.patch"
}

pkg_unpack() {
    rm -rf "$pworkdir"
    mkdir -p "$pworkdir"
    p_unpack "$psource"
}

p_install_modules() {
    mkdir -p "$kernelextramodulesdir"
    touch "$kernelmodulesdir/modules.order"
    touch "$kernelmodulesdir/modules.builtin"
    for m in "$@"; do
        local f="$PWD/${m}.ko"
        cp "$f" "$kernelextramodulesdir"
    done &&
        (
    cd $kerneldir/linux && \
        /sbin/depmod -ae -F System.map -b $INSTALL_MOD_PATH $kernelrelease
    )
}

p_depmod() {
    /sbin/depmod -ae \
        -F "$LINUX_KERNEL/System.map" \
        -b "$INSTALL_MOD_PATH" \
        "$kernelrelease"
}

p_fix_la() {
    p_run p_run sed -ri "s|libdir='/lib'|libdir='$rootfs_cross_root/lib'|" $1
}

include "$ja_libdir/env/cmake"
include "$ja_libdir/env/sdk"

pkg_build_dir="$ja_builddir/pkg"
pkg_dist_dir="$ja_libdir/dist/$ja_sdk"

distdir="$ja_libdir/dist/$ja_sdk"

pname="$1"
pstage="$2"
pconfig="$3"

plog="${ja_builddir}/${pname}-${pstage}${pconfig:+-${pconfig}}.log"
pworkdir="$pkg_build_dir/$pname"

include "$ja_libdir/pkg/$pname" 

psourcedir="${psourcedir:-${pworkdir}${psource:+/${psource}}}"
pbuilddir="${pbuilddir:-${psourcedir}}"

rm -f "$plog"
mkdir -p "$pbuilddir" && cd "$pbuilddir" || exit

stage_function="pkg_${pstage}"

if [ "$pconfig" ]; then
    use_env "$pconfig"
    stage_function="${stage_function}_${pconfig}"
fi

eval "$stage_function" \
    || die "Failed to run '$pstage' stage of package $pname ${pconfig:+($pconfig)}"
