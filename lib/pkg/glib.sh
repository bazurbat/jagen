#!/bin/sh

jagen_pkg_patch() {
    default_patch

    # leave python shebang alone
    pkg_run sed -ie '/${PYTHON}/d' \
        glib/Makefile.am glib/Makefile.in
}

jagen_pkg_build_target() {
    local cache="config.cache"
    cat >$cache <<'EOF'
glib_cv_stack_grows=no
glib_cv_uscore=yes
ac_cv_func_posix_getpwuid_r=yes
ac_cv_func_posix_getgrgid_r=yes
EOF

    default_build --cache-file="$cache"
}
