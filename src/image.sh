#!/bin/sh

pkg__image() {
    case $pkg_build_type in
        linux-kernel)
            # use_env kbuild
            pkg_run cd "$pkg_source_dir"
            pkg_run make "${pkg_build_image:?}"
            pkg_run install -vm644 \
                "$pkg_build_dir/arch/$pkg_build_arch/boot/${pkg_build_image:?}" \
                "$jagen_build_dir"
            ;;
    esac
}
