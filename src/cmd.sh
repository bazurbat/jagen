#!/bin/sh

. "$jagen_dir/src/cprint.sh"

EOF=$(printf \\034)
S=$(printf '\t')

mode=''

die() {
    unset IFS
    printf "jagen${mode:+ $mode}: %s\n" "$*"
    exit 1
}

join() {
    local sep="$1";
    printf -- "%s" "$2"
    shift 2
    if [ $# -gt 0 ]; then
        printf -- "${sep}%s" "$@"
    fi
}


assert_ninja_found() {
    if [ -z "$(command -v ninja)" ]; then
        die "could not find 'ninja' command in your PATH, please install \
Ninja (https://ninja-build.org) to run the build system."
    fi
}

cmd_build() {
    . "${jagen_include_dir:?}/root.config.sh"

    local IFS="$S"
    local build_all no_rebuild follow_selected follow_all print_all is_quiet
    local targets log logs err tries follow_pid pipe
    local build_log="${jagen_log_dir:?}/build.log"
    local outfile="${jagen_log_dir:?}/output.txt"
    local c='c'; [ -t 1 ] || c='cs'

    assert_ninja_found

    mkdir -p "$jagen_log_dir"

    while [ $# -gt 0 ]; do
        case $1 in
            --all) build_all=1 ;;
            --no-rebuild) no_rebuild=1 ;;
            --follow) follow_selected=1 ;;
            --follow-all) follow_all=1 ;;
            --progress) print_all=1 ;;
            --quiet) is_quiet=1 ;;
            --exclude) export jagen__force_exclude=1 ;;
            --clean-ignored) export jagen__clean_ignored=1 ;;
            --ignore-dirty) export jagen__ignore_dirty=1 ;;
            --ignore-exclude) export jagen__ignore_exclude=1 ;;
            -*) ;; # ignore unknown options
             *) targets="${targets}${S}${1}"
                logs="${logs}${S}${jagen_log_dir}/${1}.log" ;;
        esac
        shift
    done
    targets=${targets#$S}; logs=${logs#$S}

    export jagen_cmd_targets="$targets"
    export jagen__cmd_failed_targets_file="$jagen_build_dir/.build-failed-targets"

    cd "$jagen_build_dir" || return

    : > "$jagen__cmd_failed_targets_file" || return
    : > "$build_log" || return
    for log in $logs; do
        : > "$log" || return
    done

    if [ "$build_all" ]; then
        targets=
    fi

    if [ -z "$no_rebuild" ]; then
        rm -f $targets
    fi

    # do nothing on CTRL-C, Ninja will still receive it and exit by itself with
    # an appropriate status, we will proceed with the specific handling
    trap : INT

    if [ "$is_quiet" -o "$follow_selected" -o "$follow_all" ]; then
        export jagen__cmd_quiet=1
    elif [ "$print_all" ]; then
        export jagen__cmd_verbose=1
    fi

    if [ ! "$is_quiet" ]; then
        if [ "$follow_selected" -o "$follow_all" ]; then
            pipe=$(mktemp -u) && mkfifo "$pipe" || return
            # tee exits by itself when pipe closes after we kill tail
            tee "$outfile" <"$pipe" &
        fi
        if [ "$follow_selected" ]; then
            tail -qFc0 "$build_log" $logs >"$pipe" 2>/dev/null &
            follow_pid=$!
        elif [ "$follow_all" ]; then
            tail -qFc0 "$jagen_log_dir"/*.log >"$pipe" 2>/dev/null &
            follow_pid=$!
        fi
        if [ "$pipe" ]; then
            rm "$pipe"
        fi
    fi

    if [ "$follow_pid" ]; then
        # capture Ninja messages to a file to pass it through tail, otherwise
        # it writes to the console in parallel and mangles the output
        ninja $targets > "$build_log" 2>&1; err=$?

        # Ninja returns 2 when interrupted and discards a buffered output, we
        # assume the user wants the command line immediately, so no wait
        if [ $err != 2 ]; then
            # in the case when the final message is from Ninja we need to
            # ensure its log file ends with the EOF marker too, otherwise the
            # following loop will wait for nothing
            printf $EOF >> "$build_log"

            # when monitoring multiple files a write can end with the EOF
            # marker at the exact moment of the check while there is still more
            # data pending; we consider a probability of this negligibly small
            tries=30 # 3 seconds
            until [ "$(tail -c1 "$outfile")" = $EOF ]; do
                tries=$((tries-1)); [ $tries = 0 ] && break
                sleep 0.1
            done
        fi

        kill $follow_pid
        # waiting with redirecting stderr here allows to get rid of a
        # "Terminated: ..." message from Bash
        wait $follow_pid 2>/dev/null

        # this could happen if the terminal is very slow (WSL?), just in case
        # notify the user that the output is not complete
        if [ "$tries" = 0 ]; then
            # even after tail is killed the console might be still flushing and
            # the warning will be scrolled up out of the screen, a small delay
            # might help with that
            sleep 0.25
            ${c}println "\n{*yellow*}--{~} jagen: timed out while waiting for the console to flush, the output might be truncated"
        fi
    else
        ninja $targets; err=$?
    fi

    if [ -s "$jagen__cmd_failed_targets_file" ]; then
        local targets= num= limit=1000
        while read target log; do
            targets="${targets}${S}${target}"
            num=$(cat "$log" | wc -l)
            if [ $num -gt 0 ]; then
                ${c}println "\n{*red*}--{~} jagen: {*red*}failed target{~}: {*white*}$target"
                if [ $num -lt $limit ]; then
                    ${c}println "{*white*}--{~} $log"
                    cat "$log"
                elif [ $num -ge $limit ]; then
                    ${c}println "{*white*}--{~} the last $limit lines of $log"
                    # +1 because it counts a EOF marker as a line
                    tail -n $((limit+1)) "$log"
                fi
                ${c}println "{*white*}--{~} EOF $log"
            fi
        done < "$jagen__cmd_failed_targets_file"
        targets=${targets#$S}
        set -- $targets
        if [ $# = 1 ]; then
            ${c}println "\n{*red*}--{~} jagen: build stopped: {*red*}target failed:{~} {*white*}$targets"
        elif [ $# -gt 1 ]; then
            ${c}println "\n{*red*}--{~} jagen: build stopped: {*red*}$# targets failed:{~} {*white*}$(join '{~}, {*white*}' $targets){~}"
        fi
    fi

    return $err
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

echo_if_exists() {
    if [ -f "$1" ]; then
        echo "$1"
    fi
}

cmd_find_for_refresh() {
    local IFS="$jagen_S" paths='' dir
    for dir in bin lib src; do
        paths="${paths}${jagen_S}${jagen_dir}/${dir}"
    done
    paths="${paths}${jagen_S}${jagen_include_dir:?}"
    paths="${paths#${jagen_S}}"
    find $paths -type f -o -type d
    echo "$jagen_root_dir"
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
