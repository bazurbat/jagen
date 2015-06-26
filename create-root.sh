#!/bin/sh

root="$1"
sdk="$2"

env_file="$root/env.sh"
config_file="$root/local.sh"

die() { [ $# = 0 ] || echo "Error: $@"; exit 1; }

mkdir -p "$root" || die

cat >"$env_file" <<EOF || die
export jagen_root="$PWD"

. "\$jagen_root/lib/env.sh" ||
    { echo "Failed to load environment"; return 1; }

p_path_prepend "\$target_bin_dir"
p_path_prepend "\$pkg_private_dir/bin"
p_path_prepend "\$jagen_root/bin"
EOF

chmod +x "$env_file" || die

if [ "$sdk" ]; then
    if [ -f "$config_file" ]; then
        echo "Warning: the file '$config_file' is already exists, not overwriting"
    else
        cat >"$config_file" <<EOF || die
pkg_sdk="$sdk"
EOF
    fi
fi
