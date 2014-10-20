#!/bin/sh

psource="chibi-scheme-0.7"
pworkdir="$pworkdir${pconfig:+-$pconfig}"

set_env tools

pkg_build() {
    if [ "$pconfig" = "tools" ]; then
        p_make PREFIX="${toolsdir}${toolsprefix}"
    elif [ "$pconfig" = "target" ]; then
        set_env target
        p_make \
            PREFIX="$targetprefix" \
            CHIBI="$toolsdir/bin/chibi-scheme" \
            XCFLAGS="$targetflags"
    fi
}

pkg_install() {
    if [ "$pconfig" = "tools" ]; then
        p_make install PREFIX="${toolsdir}${toolsprefix}"
    elif [ "$pconfig" = "target" ]; then
        set_env target
        p_make \
            PREFIX="$targetprefix" \
            CHIBI="$toolsdir/bin/chibi-scheme" \
            XCFLAGS="$targetflags" \
            DESTDIR="$targetdir" \
            install
    fi
}
