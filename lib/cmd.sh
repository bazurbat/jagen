#!/bin/sh

S=$(printf '\t')

on_interrupt() { :; }

build() {
    cd "$jagen_build_dir" || exit 1
    ninja "$@"
}

cmd_run() {
    local IFS="$(printf '\n\t')"
    local arg print_targets targets_only show_output show_all
    local targets logs sts
    local cmd_log="$jagen_log_dir/build.log"

    for arg; do
        [ "$arg" = '-p' ] && { print_targets=1; continue; }
        [ "$arg" = '-t' ] && { targets_only=1; continue; }
        [ "$arg" = '-o' ] && { show_output=1; continue; }
        [ "$arg" = "-a" ] && { show_all=1; continue; }

        targets="${targets}${S}${arg}"
        logs="${logs}${S}${jagen_log_dir}/${arg}.log"
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
    help)
        . "$jagen_dir/lib/help.sh"
        print_help
        ;;
    run)
        shift
        cmd_run "$@"
        ;;
    *)
        echo "Unknown wrapper command: $1"
        exit 1
        ;;
esac
