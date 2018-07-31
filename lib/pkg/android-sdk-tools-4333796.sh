#!/bin/sh

jagen_pkg_install() {
    local sdkmanager="$pkg_source_dir/bin/sdkmanager"
    yes | "$sdkmanager" --licenses
    message "installing/updating tools using SDK Manager"
    pkg_run "$sdkmanager" "cmake;3.6.4111459"
}
