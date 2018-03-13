#!/bin/sh

S=$(printf '\t')

mode=''

die() {
    unset IFS
    printf "jagen${mode:+ $mode}: %s\n" "$*"
    exit 1
}

assert_ninja_found() {
    if [ -z "$(command -v ninja)" ]; then
        die "could not find 'ninja' command in your PATH, please install \
Ninja (https://ninja-build.org) to run the build system."
    fi
}

on_interrupt() { :; }

cmd_build() {
    local IFS="$(printf '\n\t')"
    local build_all no_rebuild show_progress show_all with_output
    local targets log logs sts build_log="${jagen_log_dir:?}/build.log"

    assert_ninja_found

    while [ $# -gt 0 ]; do
        case $1 in
            --all) build_all=1 ;;
            --no-rebuild) no_rebuild=1 ;;
            --progress) show_progress=1; with_output=1 ;;
            --all-progress) show_all=1; with_output=1 ;;
            -*) ;; # ignore unknown options
             *) targets="${targets}${S}${1}"
                logs="${logs}${S}${jagen_log_dir}/${1}.log" ;;
        esac
        shift
    done

    cd "$jagen_build_dir" || return

    : > "$build_log" || return
    for log in $logs; do
        : > "$log" || return
    done

    if [ -z "$no_rebuild" ]; then
        rm -f $targets || return
    fi

    if [ "$show_all" ]; then
        tail -qFc0 "$jagen_log_dir"/*.log 2>/dev/null &
    elif [ "$show_progress" ]; then
        tail -qFn+1 "$build_log" $logs 2>/dev/null &
    fi

    # catch SIGINT to let ninja see it and exit cleanly
    trap on_interrupt INT

    # It is hard to reliably reproduce but testing shows that both syncs are
    # necessary to avoid losing log messages from console. When neither of
    # 'show_*' options are supplied we do not sync assuming non-interactive run
    # (build server) not caring about console logs that much.

    [ "$build_all" ] && targets=
    if [ "$with_output" ]; then
        sync
        ninja $targets >"$build_log"; sts=$?
        sync
    else
        ninja $targets; sts=$?
    fi

    [ "$with_output" -a "$!" ] && kill $!

    return $sts
}

cmd_image() {
    . "$jagen_dir/env.sh" || return

    local image_script="$(find_in_path "image.sh")"

    [ "$image_script" ] ||
        die "could not find 'image.sh' in import path - \
current configuration does not support image creation"

    "${jagen_shell:-/bin/sh}" "$image_script" "$@"
}

cmd_find_patch() {
    set -u
    local name="${1:?}" filename=
    . "${jagen_dir}/env.sh" || return
    filename="${jagen_dist_dir}/patches/${name}.patch"
    if [ -f "$filename" ]; then
        echo "$filename"
        return 0
    fi
    filename="$(find_in_path "patches/${name}.patch")"
    if [ -f "$filename" ]; then
        echo "$filename"
        return 0
    fi
    return 2
}

cmd_get_path() {
    . "$jagen_dir/env.sh" || return
    local IFS="$jagen_IFS"
    jagen__expand_layers $jagen_layers
}

mode="${1:?}"
shift

case $mode in
    build)
        cmd_build "$@"
        ;;
    image)
        cmd_image "$@"
        ;;
    find_patch)
        cmd_find_patch "$@"
        ;;
    get_path)
        cmd_get_path "$@"
        ;;
    *)
        die "unknown wrapper command: $1"
        ;;
esac
