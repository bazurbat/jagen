#!/bin/sh

build() {
    cd "$pkg_build_dir" || exit 1
    ninja "$@"
}

rebuild() {
    local IFS=$(printf '\n\t')
    local tab=$(printf '\t')
    local targets_only show_all
    local target targets logs status
    local rebuild_log="rebuild.log"
    local tail_cmd="tail${tab}-qFn+1"

    cd "$pkg_build_dir" || exit 1
    : > "$rebuild_log"

    for target in "$@"; do
        [ "$target" = "-t" ] && targets_only=1
        [ "$target" = "-a" ] && show_all=1

        targets="${targets}${tab}${target}"
        logs="${logs}${tab}${target}.log"
    done

    rm -f $targets $logs

    if [ "$show_all" ]; then
        $tail_cmd *.log 2>/dev/null &
    else
        $tail_cmd $logs $rebuild_log 2>/dev/null &
    fi

    if [ "$targets_only" ]; then
        ninja $targets >$rebuild_log; status=$?
    else
        ninja >$rebuild_log; status=$?
    fi

    kill $!
    return $status
}

case $1 in
    build)
        shift
        build "$@"
        ;;
    rebuild)
        shift
        rebuild "$@"
        ;;
    *)
        echo "Unknown wrapper command: $1"
        exit 1
        ;;
esac
