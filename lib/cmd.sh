#!/bin/sh

S=$(printf '\t')

mode=''

die() {
    unset IFS
    printf "jagen${mode:+ $mode}: %s\n" "$*"
    exit 1
}

on_interrupt() { :; }

maybe_sync() {
    if [ "$show_progress" -o "$show_all" ]; then
        sync
    fi
}

cmd_build() {
    local IFS="$(printf '\n\t')"
    local dry_run show_progress show_all
    local targets logs sts
    local cmd_log="$jagen_log_dir/$mode.log"

    while [ $# -gt 0 ]; do
        case $1 in
            -n) dry_run=1       ;;
            -p) show_progress=1 ;;
            -P) show_all=1      ;;
            -*) if [ "$mode" = 'rebuild' -a "$1" = '-a' ]; then
                    build_all=1
                else
                    die "invalid option '$1', try 'jagen $mode --help'"
                fi ;;
             *) targets="${targets}${S}${1}"
                logs="${logs}${S}${jagen_log_dir}/${1}.log" ;;
        esac
        shift
    done

    if [ "$dry_run" ]; then
        set -- $targets
        if [ $# = 0 ]; then
            printf "default\n"
        else
            printf "$*\n"
        fi
        return 0
    fi

    cd "$jagen_build_dir" || return

    : > "$cmd_log" || return
    for log in $logs; do
        : > "$log" || return
    done

    if [ "$mode" = 'rebuild' ]; then
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

case $1 in
    build)
        mode="$1"
        shift
        cmd_build "$@"
        ;;
    rebuild)
        mode="$1"
        shift
        cmd_build "$@"
        ;;
    *)
        die "unknown wrapper command: $1"
        ;;
esac
