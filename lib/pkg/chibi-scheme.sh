#!/bin/sh

p_source="$p_dist_dir/chibi-scheme-0.7.tgz"
p_work_dir="$p_work_dir/build${p_config:+-$p_config}"

pkg_build_tools() {
    p_run make PREFIX="${tools_dir}${tools_prefix}"
}

pkg_install_tools() {
    p_run make PREFIX="${tools_dir}${tools_prefix}" \
        install
}

pkg_build_target() {
    use_env tools
    p_run make \
        PREFIX="$target_prefix" \
        CHIBI="$tools_dir/bin/chibi-scheme"
}

pkg_install_target() {
    use_env tools
    p_run make \
        PREFIX="$target_prefix" \
        CHIBI="$tools_dir/bin/chibi-scheme" \
        DESTDIR="$target_dir" \
        install
}
