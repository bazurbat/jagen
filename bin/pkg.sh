#!/bin/sh

if [ -z "$ja_root" ]; then
    export ja_root="$(realpath $(dirname $0)/..)"
fi

. "$ja_root/lib/env.sh" || { echo "Failed to load env"; exit 1; }
. "$ja_root/lib/pkg.sh" || die "Failed to load pkg env"

p_build_root="$ja_build_dir/pkg"

p_name="$1"
p_stage="$2"
p_config="$3"

p_log="${ja_build_dir}/${p_name}-${p_stage}${p_config:+-${p_config}}.log"
p_work_dir="$p_build_root/$p_name"

rm -f "$p_log" || die "Failed to rm $p_log"
mkdir -p "$p_work_dir" || die "Failed to mkdir $p_work_dir"
cd "$p_work_dir" || die "Failed to cd $p_work_dir"

include "$ja_lib_dir/pkg/$p_name" 

p_source_name=$(basename "$p_source" | sed -r 's/\.t(ar\.)?(gz|bz2|xz)//')
p_source_dir="${p_source_dir:-$p_work_dir/$p_source_name}"
p_build_dir="${p_build_dir:-$p_source_dir}"

mkdir -p "$p_build_dir" || die "Failed to mkdir $p_build_dir"
cd "$p_build_dir" || die "Failed to cd $p_build_dir"

p_stage_function="pkg_${p_stage}"
if [ "$p_config" ]; then
    use_env "$p_config"
    if p_is_function "${p_stage_function}_${p_config}"; then
        p_stage_function="${p_stage_function}_${p_config}"
    fi
fi

if p_is_function "$p_stage_function"; then
    eval "$p_stage_function" || \
        die "Failed to run '$p_stage' stage of package $p_name${p_config:+ ($p_config)}"
fi
