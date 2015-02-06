#!/bin/sh

p_source="git git@bitbucket.org:art-system/linux.git"
p_source_dir="$pkg_src_dir/linux"

if in_flags "new_kernel"; then
    p_source_branch="sigma-3.4"
else
    p_source_branch="ast50"
fi
