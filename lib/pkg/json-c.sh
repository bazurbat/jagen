#!/bin/sh

jagen_pkg_patch() {
    pkg_patch

    pkg_run sed -i -e "s:-Werror::" Makefile.am.inc
}
