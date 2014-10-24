#!/bin/sh

. "$ja_libdir/jagen/env.sh" || exit

p_is_function() {
    type "$1" 2>/dev/null | grep -q 'function'
}

p_cmd() {
    "$@" >>"$plog" 2>&1 || exit
}

p_unpack() {
    [ "$1" ] || die "No source"
    local A="$(ls $distdir/${1}*)"
    tar -C "$pworkdir" -xf $A
}

p_patch() {
    local patchfile="$1"
    patch -s -p1 -i "$distdir/patches/${patchfile}.patch"
    return 0
}

p_make() {
    p_cmd make "$@"
}

pkg_unpack() {
    rm -rf "$pworkdir"
    mkdir -p "$pworkdir"
    p_unpack "$psource"
}

builddir="$ja_builddir/pkg"
distdir="$ja_libdir/dist/$ja_sdk"
libdir="$ja_libdir/pkg"

pname="$1"
pstage="$2"
pconfig="$3"

plog="${ja_builddir}/${pname}-${pstage}${pconfig:+-${pconfig}}.log"
pworkdir="$builddir/$pname"

if [ -f "$libdir/${pname}.sh" ]; then
    . "$libdir/${pname}.sh"
fi

psourcedir="${psourcedir:-${pworkdir}${psource:+/${psource}}}"
pbuilddir="${pbuilddir:-${psourcedir}}"

mkdir -p "$pbuilddir" && cd "$pbuilddir" || exit

stage_function="pkg_${pstage}"

if [ "$pconfig" ]; then
    use_env "$pconfig"
    stage_function="${stage_function}_${pconfig}"
fi

eval "$stage_function" \
    || die "Failed to run '$pstage' stage of package $pname ${pconfig:+($pconfig)}"
