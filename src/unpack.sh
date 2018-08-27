#!/bin/sh

pkg__on_download_exit() {
   if [ "$pkg__current_download" ]; then
       pkg_run rm -f "$pkg__current_download"
   fi
}

pkg__download() {
    local src_path="${1:?}"
    local dest_path="${2:?}"

    pkg__current_download="$dest_path"

    trap pkg__on_download_exit EXIT

    pkg_run mkdir -p "${dest_path%/*}"
    curl -L "$src_path" -o "$dest_path" ||
        die "failed to download $src_path"

    trap - EXIT
}

pkg__path_is_uri() {
    [ "${1:?}" != "${1#*://}" ]
}

pkg__uri_is_local() {
    [ "${1:?}" != "${1#file://}" ]
}

pkg__unpack_dist() {
    local src_path="${1:?}" work_dir="${2:?}" dist_type=
    local dist_path="${jagen_dist_dir:?}/${pkg_source_filename:?}"

    if ! [ -f "$dist_path" ]; then
        if pkg__path_is_uri "$src_path"; then
            if in_flags offline && ! pkg__uri_is_local "$src_path"; then
                die "unable to download $src_path: offline mode"
            else
                pkg__download "$src_path" "$dist_path"
            fi
        else
            die "unable to unpack $dist_path: the file is not found and download location is not specified"
        fi
    fi

    dist_type=$(file -b --mime-type "$dist_path")

    if [ "$pkg_source_sha256sum" ]; then
        if [ "$(command -v sha256sum)" ]; then
            echo "$pkg_source_sha256sum $dist_path" | sha256sum -c - ||
                die "failed to verify sha256sum of $dist_path"
        else
            warning "sha256sum is not found in PATH, can not verify $pkg_name"
        fi
    elif [ "$pkg_source_sha1sum" ]; then
        if [ "$(command -v sha1sum)" ]; then
            echo "$pkg_source_sha1sum $dist_path" | sha1sum -c - ||
                die "failed to verify sha1sum of $dist_path"
        else
            warning "sha1sum is not found in PATH, can not verify $pkg_name"
        fi
    elif [ "$pkg_source_md5sum" ]; then
        if [ "$(command -v md5sum)" ]; then
            echo "$pkg_source_md5sum $dist_path" | md5sum -c - ||
                die "failed to verify md5sum of $dist_path"
        else
            warning "md5sum is not found in PATH, can not verify $pkg_name"
        fi
    fi

    [ -f "$dist_path" ] ||
        die "could not find $dist_path for unpacking"

    pkg_run mkdir -p "$work_dir"
    pkg_run cd "$work_dir"

    case $dist_type in
        application/x-sharedlib)
            pkg_run chmod +x "$dist_path"
            return ;;
        */zip)
            pkg_run unzip "$dist_path"
            return ;;
    esac

    case $pkg_source_filename in
        *.tar|*.tar.*|*.tgz|*.tbz2|*.txz)
            pkg_run tar -xf "$dist_path" ;;
    esac
}

pkg_unpack() {
    local IFS; unset IFS
    set -- $pkg_source
    local src_type="$1"
    local src_path="$2"

    case $src_type in
        dist)
            pkg__unpack_dist "$src_path" "$pkg_work_dir"
            ;;
        git|hg|repo)
            if [ -d "$pkg_source_dir" ] && ! jagen__is_empty "$pkg_source_dir" && \
               [ "$pkg_source_exclude" ]; then
                message "not updating $pkg_name: the source is excluded"
            else
                _jagen src update "$pkg_name" || return
            fi
            ;;
    esac
}
