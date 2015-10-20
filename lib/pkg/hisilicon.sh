#!/bin/sh

pkg_build() {
    use_env lunch || return
    p_run make bigfish_emmc
}
