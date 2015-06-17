#!/bin/sh

build() {
    cd "$pkg_build_dir" || exit 1
    ninja "$@" | tee "build.log"
}

rebuild() {
    local IFS=$(printf '\n\t')
    local tab=$(printf '\t')
    local targets_only show_all
    local target targets logs status
    local rebuild_log="rebuild.log"

    cd "$pkg_build_dir" || exit 1
    : > "$rebuild_log"

    for target in "$@"; do
        [ "$target" = "-t" ] && { targets_only=1; continue; }
        [ "$target" = "-a" ] && { show_all=1; continue; }

        targets="${targets}${tab}${target}"
        logs="${logs}${tab}${target}.log"
    done

    rm -f $targets

    if [ "$show_all" ]; then
        tail -qFn0 *.log 2>/dev/null &
    else
        rm -f $logs
        tail -qFn+1 $logs $rebuild_log 2>/dev/null &
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
