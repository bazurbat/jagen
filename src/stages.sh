#!/bin/sh

. "$jagen_dir/env.sh" || { echo "Failed to load environment"; exit 1; }

# Disable passphrase querying
export GIT_SSH_COMMAND="ssh -o BatchMode=yes"
# Do not prompt on the terminal (e.g. when asking for HTTP credentials).
export GIT_TERMINAL_PROMPT=0
# Never install the translations.
export LINGUAS=""

jagen_build_args_file="${jagen_build_dir:?}/.build-args"

include "$jagen__src_dir/util"
include "$jagen__src_dir/unpack"
include "$jagen__src_dir/patch"
include "$jagen__src_dir/configure"
include "$jagen__src_dir/compile"
include "$jagen__src_dir/install"
include "$jagen__src_dir/image"

jagen_stage_unpack() {
    _jagen clean "${pkg_name:?}"
    pkg_unpack
}

jagen_stage_patch() {
    pkg_patch
    pkg_patch_copy_files
}

jagen_stage_autoreconf() {
    pkg_autoreconf
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

jagen_stage_export() {
    local prefix= content= key=
    local name="$(jagen_name_to_id "$pkg_name")"
    local outfile="$(pkg__export_fname "$pkg_name" "$pkg_config")"
    if [ "$pkg_config" ]; then
        prefix="pkg__${pkg_config}__export"
        content="${name}_install_dir='$pkg_install_dir'"
    else
        prefix="pkg_export"
    fi
    for key in $(set | jagen_esed -n "s/^${prefix}_([[:alnum:]_]+)=.*/\1/p"); do
        content="${content}${jagen_S}${name}_${key}='$(eval echo \"\$${prefix}_${key}\")'"
    done
    content=${content#$jagen_S}
    if [ "$content" ]; then
        echo "$content" > "$outfile"
    fi
}
