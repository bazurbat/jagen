#!/bin/sh

jagen_pkg_install() {
    local name="commonapi-dbus-generator-linux-x86_64"
    pkg_link "$pkg_work_dir/$pkg_source_basename/$name" "$pkg_install_dir/bin/$name"
}
