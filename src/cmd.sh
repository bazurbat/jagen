#!/bin/sh

S=$(printf '\t')

mode=''
tail_pid=

die() {
    unset IFS
    printf "jagen${mode:+ $mode}: %s\n" "$*"
    exit 1
}

is_wsl() {
    grep -q Microsoft /proc/version 2>&-
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
    local build_all no_rebuild follow_selected follow_all show_all is_quiet
    local targets log logs sts build_log="${jagen_log_dir:?}/build.log"

    assert_ninja_found

    while [ $# -gt 0 ]; do
        case $1 in
            --all) build_all=1 ;;
            --no-rebuild) no_rebuild=1 ;;
            --progress) show_all=1 ;;
            --follow) follow_selected=1 ;;
            --follow-all) follow_all=1 ;;
            --quiet) is_quiet=1 ;;
            --ignore-dirty) export jagen__ignore_dirty=1 ;;
            --ignore-exclude) export jagen__ignore_exclude=1 ;;
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

    [ "$build_all" ] && targets=

    if [ -z "$no_rebuild" ]; then
        rm -f $targets || return
    fi

    trap 'exit 2' INT
    trap cleanup EXIT

    if [ "$is_quiet" -o "$follow_all" ]; then
        export jagen__stage_quiet=1
    elif [ "$show_all" ]; then
        export jagen__stage_verbose=1
    fi

    if [ ! "$is_quiet" ]; then
        if [ "$follow_selected" ]; then
            tail -qFc0 "$build_log" $logs 2>/dev/null &
            tail_pid=$!
            export jagen__cmd_logs="$logs"
        elif [ "$follow_all" ]; then
            tail -qFc0 "$jagen_log_dir"/*.log 2>/dev/null &
            tail_pid=$!
        fi
    fi

    if [ "$tail_pid" ]; then
        # redirecting the output to the build log is needed to let tail buffer
        # it because otherwise when ninja writes to the console at the same
        # time as tail the lines often mix with each other
        ninja $targets >"$build_log" 2>&1; sts=$?
        if is_wsl; then
            # the console on Windows is really slow
            sleep 0.5
        else
            sleep 0.1
        fi
    else
        ninja $targets; sts=$?
    fi

    return $sts
}

cmd_image() {
    . "$jagen_dir/src/common.sh" || return
    local image_script="$(find_in_path "image.sh")"
    [ "$image_script" ] ||
        die "the current project does not support image creation"
    "${jagen_shell:-/bin/sh}" "$image_script" "$@"
}

cmd_find_in_path() {
    local arg='' path='' result=''
    . "${jagen_dir:?}/src/common.sh" || return
    set -- "$@"
    for arg; do
        path=$(find_in_path "$arg")
        if [ -z "$path" ]; then path="$arg"; fi
        result="${result}${jagen_S}${path}"
    done
    result=${result#$jagen_S}
    printf '%s' "$result"
}

cmd_find_files() {
    local name="$1" prefix="${1%~*}" rv= arg path; shift
    set -- "$@"
    . "${jagen_dir:?}/src/common.sh" || return
    for arg; do
        if [ "$prefix" = "$name" ]; then
            path=$(find_in_path "pkg/$name/$arg")
        else
            path=$(find_in_path "pkg/$name/$arg" "pkg/$prefix/$arg")
        fi
        [ "$path" ] || path="$arg"
        rv="$rv$jagen_S$path"
    done
    rv=${rv#$jagen_S}
    printf '%s' "$rv"
}

echo_if_exists() {
    if [ -f "$1" ]; then
        echo "$1"
    fi
}

cmd_find_for_refresh() {
    . "$jagen_dir/src/common.sh" || return
    local paths="$jagen_dir" IFS="$jagen_S"
    paths="${paths}${jagen_S}$(jagen__resolve_layers)" || return
    if [ -d "$jagen_root_lib_dir" ]; then
        paths="${paths}${jagen_S}${jagen_root_lib_dir}"
    fi
    find $paths '(' \
        -name '.git' -o \
        -path '*jagen/doc' -o \
        -name tags -o \
        -name Session.vim \
    ')' -prune -o -print
    echo "$jagen_root_dir"
    echo_if_exists "$jagen_root_dir/config.sh"
    echo_if_exists "$jagen_root_dir/env.sh"
    echo_if_exists "$jagen_root_dir/jagen"
    echo_if_exists "$jagen_root_dir/rules.lua"
}

cmd_get_path() {
    . "$jagen_dir/src/common.sh" || return
    jagen__get_path
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
    find_files)
        cmd_find_files "$@"
        ;;
    find_for_refresh)
        cmd_find_for_refresh "$@"
        ;;
    get_path)
        cmd_get_path "$@"
        ;;
    *)
        die "unknown wrapper command: $1"
        ;;
esac
