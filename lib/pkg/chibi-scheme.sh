#!/bin/sh

p_source="$p_dist_dir/chibi-scheme-0.7.tgz"
p_work_dir="$p_work_dir${p_config:+-$p_config}"

pkg_unpack_tools() {
    pkg_unpack "$@"
}

pkg_build_tools() {
    p_run make PREFIX="${toolsdir}${toolsprefix}"
}

pkg_install_tools() {
    p_run make install PREFIX="${toolsdir}${toolsprefix}"
}

pkg_build_target() {
    use_env tools
    p_run make \
        PREFIX="$targetprefix" \
        CHIBI="$toolsdir/bin/chibi-scheme" \
        XCFLAGS="$targetflags"
}

pkg_install_target() {
    p_run make \
        PREFIX="$targetprefix" \
        CHIBI="$toolsdir/bin/chibi-scheme" \
        XCFLAGS="$targetflags" \
        DESTDIR="$targetdir" \
        install
}
