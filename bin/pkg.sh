#!/bin/sh

if [ -z "$ja_basedir" ]; then
    export ja_basedir="$(realpath $(dirname $0)/..)"
fi

. "$ja_basedir/lib/jagen/env.sh" || exit
. "$ja_basedir/lib/jagen/pkg.sh" || exit

include "$ja_libdir/env/cmake"
include "$ja_libdir/env/sdk"

p_build_root="$ja_builddir/pkg"

p_name="$1"
p_stage="$2"
p_config="$3"

p_log="${ja_builddir}/${p_name}-${p_stage}${p_config:+-${p_config}}.log"
p_work_dir="$p_build_root/$p_name"

include "$ja_libdir/pkg/$p_name" 

if [ -z "$p_source_dir" ]; then
    p_source_name=$(basename "$p_source" \
        | sed -r 's/\.t(ar\.)?(gz|bz2|xz)//')
    p_source_dir="$p_work_dir/$p_source_name"
fi

p_build_dir="${p_build_dir:-${p_source_dir}}"

rm -f "$p_log"
mkdir -p "$p_build_dir" && cd "$p_build_dir" || exit

p_stage_function="pkg_${p_stage}"

if [ "$p_config" ]; then
    use_env "$p_config"
    p_stage_function="${p_stage_function}_${p_config}"
fi

eval "$p_stage_function" || \
    die "Failed to run '$p_stage' stage of package $p_name${p_config:+ ($p_config)}"
