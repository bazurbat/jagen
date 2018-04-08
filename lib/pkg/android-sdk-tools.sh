#!/bin/sh

jagen_pkg_install() {
    local sdkmanager="$pkg_source_dir/bin/sdkmanager"
    yes | "$sdkmanager" --licenses
    pkg_run "$sdkmanager" tools "cmake;3.6.4111459"
}
