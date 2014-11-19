#!/bin/sh

export ja_root=$(realpath .)

. "$ja_root/lib/env.sh" ||
    { echo "Failed to load environment"; exit 1; }

PATH="$ja_root/bin:$ja_files_dir/bin:$PATH"
