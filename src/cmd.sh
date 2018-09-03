#!/bin/sh

S=$(printf '\t')

mode=''
tail_pid=

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

cleanup() {
    if [ "$tail_pid" ]; then
        kill "$tail_pid"
        tail_pid=
    fi
}

cmd_build() {
    local IFS="$(printf '\n\t')"
    local build_all no_rebuild show_progress is_quiet with_output
    local targets log logs sts build_log="${jagen_log_dir:?}/build.log"

    assert_ninja_found

    while [ $# -gt 0 ]; do
        case $1 in
            --all) build_all=1 ;;
            --no-rebuild) no_rebuild=1 ;;
            --progress) show_progress=1 ;;
            --quiet) is_quiet=1 ;;
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

    if ! [ "$is_quiet" ]; then
        if [ "$show_progress" ]; then
            tail -qFc0 "$jagen_log_dir"/*.log 2>/dev/null &
            with_output=1
            tail_pid=$!
        elif [ "$logs" ]; then
            tail -qFn+1 "$build_log" $logs 2>/dev/null &
            with_output=1
            tail_pid=$!
        fi
    fi

    trap cleanup INT

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

    cleanup

    return $sts
}

cmd_image() {
    . "$jagen_dir/env.sh" || return
    local image_script="$(find_in_path "image.sh")"
    [ "$image_script" ] ||
        die "the current project does not support image creation"
    "${jagen_shell:-/bin/sh}" "$image_script" "$@"
}

cmd_find_in_path() {
    local arg='' path='' result=''
    . "${jagen_dir:?}/env.sh" || return
    set -- "$@"
    for arg; do
        path=$(find_in_path "$arg")
        if [ -z "$path" ]; then path="$arg"; fi
        result="${result}${jagen_S}${path}"
    done
    result=${result#$jagen_S}
    printf '%s' "$result"
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
    find)
        cmd_find_in_path "$@"
        ;;
    get_path)
        cmd_get_path "$@"
        ;;
    *)
        die "unknown wrapper command: $1"
        ;;
esac
