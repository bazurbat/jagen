#!/bin/sh

psource="chibi-scheme-0.7"
pworkdir="$pworkdir${pconfig:+-$pconfig}"

pkg_unpack_tools() {
    pkg_unpack "$@"
}

pkg_build_tools() {
    p_make PREFIX="${toolsdir}${toolsprefix}"
}

pkg_install_tools() {
    p_make install PREFIX="${toolsdir}${toolsprefix}"
}

pkg_build_target() {
    use_env tools
    p_make \
        PREFIX="$targetprefix" \
        CHIBI="$toolsdir/bin/chibi-scheme" \
        XCFLAGS="$targetflags"
}

pkg_install_target() {
    p_make \
        PREFIX="$targetprefix" \
        CHIBI="$toolsdir/bin/chibi-scheme" \
        XCFLAGS="$targetflags" \
        DESTDIR="$targetdir" \
        install
}
