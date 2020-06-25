# detecting if the standard output is connected to a terminal does not work
# from a subshell so we need to use separate functions

cprint_gsub() {
    [ $# = 0 ] && return
    echo "$@{reset}" | sed \
        -e 's#{reset}#\\033[0m#g'      -e 's#{~}#\\033[0m#g'  \
        -e 's#{bold}#\\033[1m#g'       -e 's#{@}#\\033[1m#g'  \
        -e 's#{dim}#\\033[2m#g'        -e 's#{h}#\\033[2m#g'  \
        -e 's#{italic}#\\033[3m#g'     -e 's#{i}#\\033[3m#g'  \
        -e 's#{underline}#\\033[4m#g'  -e 's#{u}#\\033[4m#g'  \
        -e 's#{normal}#\\033[22m#g'    -e 's#{n}#\\033[22m#g' \
        -e 's#{black}#\\033[30m#g'     -e 's#{k}#\\033[30m#g' \
        -e 's#{red}#\\033[31m#g'       -e 's#{r}#\\033[31m#g' \
        -e 's#{green}#\\033[32m#g'     -e 's#{g}#\\033[32m#g' \
        -e 's#{brown}#\\033[33m#g'     -e 's#{o}#\\033[33m#g' \
        -e 's#{blue}#\\033[34m#g'      -e 's#{b}#\\033[34m#g' \
        -e 's#{magenta}#\\033[35m#g'   -e 's#{m}#\\033[35m#g' \
        -e 's#{cyan}#\\033[36m#g'      -e 's#{y}#\\033[36m#g' \
        -e 's#{white}#\\033[37m#g'     -e 's#{w}#\\033[37m#g' \
        -e 's#{blackbg}#\\033[40m#g'   -e 's#{K}#\\033[40m#g' \
        -e 's#{redbg}#\\033[41m#g'     -e 's#{R}#\\033[41m#g' \
        -e 's#{greenbg}#\\033[42m#g'   -e 's#{G}#\\033[42m#g' \
        -e 's#{brownbg}#\\033[43m#g'   -e 's#{O}#\\033[43m#g' \
        -e 's#{bluebg}#\\033[44m#g'    -e 's#{B}#\\033[44m#g' \
        -e 's#{magentabg}#\\033[45m#g' -e 's#{M}#\\033[45m#g' \
        -e 's#{cyanbg}#\\033[46m#g'    -e 's#{Y}#\\033[46m#g' \
        -e 's#{whitebg}#\\033[47m#g'   -e 's#{W}#\\033[47m#g' \
        -e 's#{defbg}#\\033[49m#g'     -e 's#{D}#\\033[49m#g'
}

csprint_gsub() {
    [ $# = 0 ] && return
    echo "$@" | sed \
        -e 's#{reset}##g'     -e 's#{~}##g' \
        -e 's#{bold}##g'      -e 's#{@}##g' \
        -e 's#{dim}##g'       -e 's#{h}##g' \
        -e 's#{italic}##g'    -e 's#{i}##g' \
        -e 's#{underline}##g' -e 's#{u}##g' \
        -e 's#{normal}##g'    -e 's#{n}##g' \
        -e 's#{black}##g'     -e 's#{k}##g' \
        -e 's#{red}##g'       -e 's#{r}##g' \
        -e 's#{green}##g'     -e 's#{g}##g' \
        -e 's#{brown}##g'     -e 's#{o}##g' \
        -e 's#{blue}##g'      -e 's#{b}##g' \
        -e 's#{magenta}##g'   -e 's#{m}##g' \
        -e 's#{cyan}##g'      -e 's#{y}##g' \
        -e 's#{white}##g'     -e 's#{w}##g' \
        -e 's#{blackbg}##g'   -e 's#{K}##g' \
        -e 's#{redbg}##g'     -e 's#{R}##g' \
        -e 's#{greenbg}##g'   -e 's#{G}##g' \
        -e 's#{brownbg}##g'   -e 's#{O}##g' \
        -e 's#{bluebg}##g'    -e 's#{B}##g' \
        -e 's#{magentabg}##g' -e 's#{M}##g' \
        -e 's#{cyanbg}##g'    -e 's#{Y}##g' \
        -e 's#{whitebg}##g'   -e 's#{W}##g' \
        -e 's#{defbg}##g'     -e 's#{D}##g'
}

cprint() {
    local O
    # prevent command substitution from stripping ending newlines from the
    # result by padding it with a character
    O=$(cprint_gsub "$@\034")
    # trim the added character to keep the arguments verbatim
    printf -- "${O%\034}"
}

csprint() {
    local O="$(csprint_gsub "$@\034")"
    printf -- "${O%\034}"
}

cprintln() {
    # if a reset is not performed before the final newline it can lead to
    # graphical artifacts such as leaving the cursor with the wrong color or
    # the last background color leaking from the previous line
    cprint "$@{reset}\n"
}

csprintln() {
    # no reset beacuse "sc" variant just strips control sequences anyway
    csprint "$@\n"
}
