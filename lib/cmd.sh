#!/bin/sh

S=$(printf '\t')

die() {
    unset IFS
    printf 'jagen run: %s\n' "$*"
    exit 1
}

on_interrupt() { :; }

cmd_run() {
    local IFS="$(printf '\n\t')"
    local print_targets targets_only show_output show_all
    local targets logs sts
    local cmd_log="$jagen_log_dir/build.log"

    while [ $# -gt 0 ]; do
        case $1 in
            -p) print_targets=1 ;;
            -t) targets_only=1 ;;
            -o) show_output=1 ;;
            -a) show_all=1 ;;
            -*) die "invalid option '$1', try 'jagen run --help'" ;;
             *) targets="${targets}${S}${1}"
                logs="${logs}${S}${jagen_log_dir}/${1}.log" ;;
        esac
        shift
    done

    if [ "$print_targets" ]; then
        set -- $targets
        if [ $# = 0 ]; then
            printf "default\n"
        else
            printf "$*\n"
        fi
        return 0
    fi

    cd "$jagen_build_dir" || exit 1
    : > "$cmd_log"

    rm -f $targets
    for log in $logs; do
        : > "$log"
    done

    if [ "$show_output" ]; then
        tail -qFn+1 "$cmd_log" $logs 2>/dev/null &
    elif [ "$show_all" ]; then
        tail -qFn0 *.log 2>/dev/null &
    else
        tail -qFn+1 "$cmd_log" &
    fi

    # catch SIGINT to kill background tail process and exit cleanly
    trap on_interrupt INT

    if [ "$targets_only" ]; then
        ninja $targets > "$cmd_log"; sts=$?
    else
        ninja > "$cmd_log"; sts=$?
    fi

    kill $!

    return $sts
}

case $1 in
    run)
        shift
        cmd_run "$@"
        ;;
    *)
        die "unknown wrapper command: $1"
        ;;
esac
