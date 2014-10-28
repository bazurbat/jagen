#!/bin/sh

if [ -z "$ja_basedir" ]; then
    export ja_basedir="$(realpath $(dirname $0)/..)"
fi

. "$ja_basedir/lib/jagen/env.sh" || exit
. "$ja_basedir/lib/jagen/pkg.sh" || exit

include "$ja_libdir/env/cmake"
include "$ja_libdir/env/sdk"

pkg_builddir="$ja_builddir/pkg"
pkg_distdir="$ja_libdir/dist/$ja_sdk"

pname="$1"
pstage="$2"
pconfig="$3"

plog="${ja_builddir}/${pname}-${pstage}${pconfig:+-${pconfig}}.log"
pworkdir="$pkg_builddir/$pname"

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

eval "$stage_function" || \
    die "Failed to run '$pstage' stage of package $pname${pconfig:+ ($pconfig)}"
