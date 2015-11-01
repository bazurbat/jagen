#!/bin/sh

. "$jagen_dir/lib/env.sh" ||
    { echo "Failed to load environment"; exit 1; }

: ${pkg_run_jobs:=$(nproc)}
: ${pkg_run_on_error:=exit}

pkg_run() {
    local cmd="$1"
    debug "$*"
    shift

    case $cmd in
        make)
            cmd="$cmd -j$pkg_run_jobs"
            [ "$jagen_build_verbose" = "yes" ] && cmd="$cmd V=1"
            ;;
        ninja)
            cmd="$cmd -j$pkg_run_jobs"
            [ "$jagen_build_verbose" = "yes" ] && cmd="$cmd -v"
            ;;
    esac

    $cmd "$@" || $pkg_run_on_error
}

pkg_clean_dir() {
    local dir="$1"
    if [ -d "$dir" ]; then
        rm -rf "$dir"/* ||
            die "Failed to clean directory: $dir"
    else
        mkdir -p "$dir" ||
            die "Failed to create directory: $dir"
    fi
}

pkg_strip_dir() {
    local root files
    root="$1"
    files=$(find "$root" -type f -not -name "*.ko" \
        "(" -path "*/lib*" -o -path "*/bin*" -o -path "*/sbin*" ")" | \
        xargs -r file | grep "ELF.*\(executable\|shared object\).*not stripped" | cut -d: -f1)

    for f in $files; do
        pkg_run "$STRIP" -v --strip-unneeded \
            -R .comment \
            -R .GCC.command.line \
            -R .note.gnu.gold-version \
            "$f"
    done
}

pkg_run_patch() {
    pkg_run patch -p${1} -i "$jagen_patch_dir/${2}.patch"
}

pkg_install_modules() {
    mkdir -p "$kernel_extra_modules_dir"
    touch "$kernel_modules_dir/modules.order"
    touch "$kernel_modules_dir/modules.builtin"
    for m in "$@"; do
        local f="$PWD/${m}.ko"
        cp "$f" "$kernel_extra_modules_dir"
    done &&
        (
    cd $kernel_dir/linux && \
        /sbin/depmod -ae -F System.map -b $INSTALL_MOD_PATH $kernel_release
    )
}

pkg_run_depmod() {
    pkg_run /sbin/depmod -ae \
        -F "$LINUX_KERNEL/System.map" \
        -b "$INSTALL_MOD_PATH" \
        "$kernel_release"
}

pkg_fix_la() {
    local filename="$1"
    local prefix=${2:-"$sdk_rootfs_prefix"}
    debug "fix la $filename $prefix"
    pkg_run sed -i "s|^\(libdir=\)'\(.*\)'$|\1'${prefix}\2'|" "$filename"
}

pkg_run_autoreconf() {
    pkg_run autoreconf -if -I "$jagen_host_dir/share/aclocal"
}

jagen_pkg_clean() {
    set -- $pkg_source
    local kind="$1"

    case $kind in
        git|hg)
            if in_list "$pkg_name" $jagen_source_exclude; then
                message "pkg source '$pkg_name' excluded from cleaning"
            elif [ -d "$pkg_source_dir" ]; then
                _jagen src clean "$pkg_name"
            fi
            ;;
    esac

    pkg_clean_dir "$pkg_work_dir"
}

jagen_pkg_unpack() {
    set -- $pkg_source
    local kind="$1"
    local src="${2:-$1}"

    [ "$pkg_source" ] || return 0

    case $kind in
        git|hg)
            if in_flags "offline"; then
                message "Offline mode, not checking $pkg_name"
            elif in_list "$pkg_name" $jagen_source_exclude; then
                message "pkg source '$pkg_name' excluded from pulling"
            elif [ -d "$pkg_source_dir" ]; then
                if _jagen src dirty "$pkg_name"; then
                    warning "$pkg_source_dir is dirty, not updating"
                else
                    _jagen src update "$pkg_name"
                fi
            else
                _jagen src clone "$pkg_name"
            fi
            ;;
        *)
            pkg_run tar -C "$pkg_work_dir" -xf "$src"
            ;;
    esac
}

default_patch() {
    if [ ! -x "$pkg_source_dir/configure" -a -x "$pkg_source_dir/autogen.sh" ]; then
        "$pkg_source_dir/autogen.sh"
    fi
}

jagen_pkg_configure() {
    if [ "$pkg_with_provided_libtool" ]; then
        pkg_run_autoreconf
    fi
}

jagen_pkg_build_pre() {
    [ -d "$pkg_build_dir" ] || pkg_run mkdir -p "$pkg_build_dir"
    pkg_run cd "$pkg_build_dir"
}

default_build() {
    if [ -x "$pkg_source_dir/configure" ]; then
        pkg_run "$pkg_source_dir/configure" \
            --host="$pkg_system" \
            --prefix="$pkg_prefix" \
            $pkg_options
        pkg_run make
    fi
}

jagen_pkg_install_pre() {
    # for packages that do not have build stage
    jagen_pkg_build_pre
}

default_install() {
    pkg_run make DESTDIR="$pkg_dest_dir" install

    for name in $pkg_libs; do
        pkg_fix_la "$pkg_dest_dir$pkg_prefix/lib/lib${name}.la" "$pkg_dest_dir"
    done
}

jagen_pkg_patch() { default_patch; }

jagen_pkg_build() { default_build; }

jagen_pkg_install() { default_install; }
