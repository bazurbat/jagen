#!/bin/sh

. "$jagen_dir/env.sh" || { echo "Failed to load environment"; exit 1; }

jagen_build_args_file="${jagen_build_dir:?}/.build-args"

include "$jagen__src_dir/util"
include "$jagen__src_dir/unpack"
include "$jagen__src_dir/patch"
include "$jagen__src_dir/configure"
include "$jagen__src_dir/compile"
include "$jagen__src_dir/install"
include "$jagen__src_dir/image"

jagen_stage_clean() {
    pkg_clean
}

jagen_stage_unpack() {
    pkg_unpack
}

jagen_stage_update() {
    pkg_unpack
}

jagen_stage_patch() {
    pkg_patch
    pkg_patch_copy_files
    pkg_generate
}

jagen_stage_configure() {
    pkg_configure
}

jagen_stage_compile() {
    pkg_compile
}

jagen_stage_install() {
    pkg_install
}

jagen_stage_image() {
    pkg__image
}
