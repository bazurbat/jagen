#!/bin/sh

p_is_function() {
    type "$1" 2>/dev/null | grep -q 'function'
}

p_run() {
    debug "$*"
    "$@" >>"$plog" 2>&1 || exit
}

p_clean() {
    if [ -d "$1" ]; then
        p_run rm -fr "$1"
    fi
    p_run mkdir -p "$1"
}

p_strip() {
    local root files
    root="$1"
    files=$(find "$root" -type f -not -name "*.ko" \
        "(" -path "*/lib*" -o -path "*/bin*" -o -path "*/sbin*" ")" | \
        xargs -r file | grep "ELF.*\(executable\|shared object\).*not stripped" | cut -d: -f1)

    for f in $files; do
        "$STRIP" -v --strip-unneeded \
            -R .comment \
            -R .GCC.command.line \
            -R .note.gnu.gold-version \
            "$f"
    done
}

p_unpack() {
    [ "$1" ] || die "No source"
    local A="$(ls $pkg_distdir/${1}*)"
    tar -C "$pworkdir" -xf $A
}

p_patch() {
    p_run patch -sp1 -i "$pkg_distdir/patches/${1}.patch"
}

p_install_modules() {
    mkdir -p "$kernelextramodulesdir"
    touch "$kernelmodulesdir/modules.order"
    touch "$kernelmodulesdir/modules.builtin"
    for m in "$@"; do
        local f="$PWD/${m}.ko"
        cp "$f" "$kernelextramodulesdir"
    done &&
        (
    cd $kerneldir/linux && \
        /sbin/depmod -ae -F System.map -b $INSTALL_MOD_PATH $kernelrelease
    )
}

p_depmod() {
    /sbin/depmod -ae \
        -F "$LINUX_KERNEL/System.map" \
        -b "$INSTALL_MOD_PATH" \
        "$kernelrelease"
}

p_fix_la() {
    p_run p_run sed -ri "s|libdir='/lib'|libdir='$sdk_rootfs_prefix/lib'|" $1
}

p_git_clone() {
    local directory="$1" address="$2"

    p_run git clone --progress "$address" "$directory"
}

p_git_checkout() {
    local directory="$1" branch="${2:-master}"

    p_run cd "$directory"
    [ "$(git status --porcelain)" ] || p_run git checkout "$branch"
    p_run cd -
}

p_git_update() {
    local directory="$1"

    p_run cd "$directory"
    [ "$(git status --porcelain)" ] || p_run git pull --progress
    p_run cd -
}

pkg_clean() {
    p_run rm -rf "$pworkdir"/*
}

pkg_unpack() {
    if [ "$p_type" = "git" ]; then
        if [ -d "$psourcedir" ]; then
            p_git_update "$psourcedir"
        else
            p_git_clone "$psourcedir" "$p_address"
            p_git_checkout "$psourcedir" "$p_branch"
        fi
    else
        rm -rf "$pworkdir"
        mkdir -p "$pworkdir"
        p_unpack "$psource"
    fi
}
