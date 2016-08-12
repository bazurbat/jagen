#!/bin/sh

jagen_pkg_configure() {
    # Workaround from: https://sourceware.org/bugzilla/show_bug.cgi?id=18113#c1
    cat > makeinfo <<'EOF' || return
#!/bin/sh
echo "makeinfo (GNU texinfo) 5.2"
EOF
    pkg_run chmod a+x makeinfo

    pkg_configure MAKEINFO="$(realpath ./makeinfo)"
}
