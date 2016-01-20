#!/bin/sh

jagen_pkg_build() {
    export ac_cv_lib_pthread_pthread_create=no

    default_build
}
