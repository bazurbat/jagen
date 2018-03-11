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
        die "A 'ninja' command is not found in your PATH. You need to install \
Ninja build system (https://ninja-build.org) to run 'build' or 'rebuild'"
    fi
}

on_interrupt() { :; }

maybe_sync() {
    if [ "$show_progress" -o "$show_all" ]; then
        sync
    fi
}

cmd_build() {
    local IFS="$(printf '\n\t')"
    local dry_run show_progress show_all build_force build_all
    local targets logs sts arg i
    local cmd_log="$jagen_log_dir/$mode.log"

    assert_ninja_found

    while [ $# -gt 0 ]; do
        case $1 in
            --dry-run) dry_run=1 ;;
            --progress) show_progress=1 ;;
            --all-progress) show_all=1 ;;
            --force) build_force=1 ;;
            --all) build_all=1 ;;
            -*) ;; # ignore unknown options
             *) targets="${targets}${S}${1}"
                logs="${logs}${S}${jagen_log_dir}/${1}.log" ;;
        esac
        shift
    done

    if [ "$dry_run" ]; then
        set -- $targets
        if [ $# != 0 ]; then
            printf "$*\n"
        fi
        return 0
    fi

    cd "$jagen_build_dir" || return

    : > "$cmd_log" || return
    for log in $logs; do
        : > "$log" || return
    done

    if [ "$build_force" ]; then
        rm -f $targets || return
    fi

    if [ "$show_progress" ]; then
        tail -qFn+1 "$cmd_log" $logs 2>/dev/null &
    elif [ "$show_all" ]; then
        tail -qFn0 *.log 2>/dev/null &
    else
        tail -qFn+1 "$cmd_log" &
    fi

    # catch SIGINT to kill background tail process and exit cleanly
    trap on_interrupt INT

    # It is hard to reliably reproduce but testing shows that both syncs are
    # necessary to avoid losing log messages from console. When neither of
    # 'show_*' options are supplied we do not sync assuming non-interactive run
    # (build server) not caring about console logs that much.

    maybe_sync
    if [ "$build_all" ]; then
        ninja > "$cmd_log"; sts=$?
    else
        ninja $targets > "$cmd_log"; sts=$?
    fi
    maybe_sync

    kill $!

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
