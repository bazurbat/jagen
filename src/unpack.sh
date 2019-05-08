#!/bin/sh

pkg__is_scm() {
    case $pkg_source_type in
        git|hg|repo) return 0 ;;
    esac
    return 1
}

pkg__download_cleanup() {
    pkg__rm $pkg__current_download
    pkg__current_download=
}

pkg__download() {
    local src_path="${1:?}"
    local dest_path="${2:?}"
    local cookie_path= confirm_key=

    pkg_run mkdir -p "${dest_path%/*}"

    if [ "$pkg_source_type" = "dist:gdrive" ]; then
        cookie_path=$(mktemp /tmp/jagen-cookie.XXXXXXXX)
        [ "$cookie_path" ] || die "failed to create a temp file for storing cookies"

        pkg__current_download="$cookie_path $dest_path"
        pkg__curl -c "$cookie_path" "https://drive.google.com/uc?export=download&id=$src_path" -o "$dest_path"

        confirm_key=$(awk '$1 ~ /#HttpOnly_.drive.google.com/ && $6 ~ /^download_warning_/ { print $NF }' "$cookie_path")
        if [ "$confirm_key" ]; then
            pkg__curl "https://drive.google.com/uc?export=download&confirm=$confirm_key&id=$src_path" -o "$dest_path"
        fi
    else
        pkg__current_download="$dest_path"
        pkg__curl "$src_path" -o "$dest_path"
    fi

    # cleanup only cookie if set
    pkg__current_download="$cookie_path"
}

pkg__path_is_uri() {
    [ "${1:?}" != "${1#*://}" ]
}

pkg__uri_is_local() {
    [ "${1:?}" != "${1#file://}" ]
}

pkg__unpack_dist() {
    local src_path="${1:?}" dest_dir="${2:?}" dist_type=
    local dist_path="${jagen_dist_dir:?}/${pkg_source_filename:?}"

    if ! [ -f "$dist_path" ]; then
        if [ "$pkg_source_type" = "dist:gdrive" ] || pkg__path_is_uri "$src_path"; then
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

    [ -d "$dest_dir" ] || pkg_run mkdir -p "$dest_dir"
    pkg_run cd "$dest_dir"

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

pkg_clean() {
    local dir toclean= IFS="$jagen_S"
    if [ "$pkg_build_clean" ]; then
        for dir in $pkg_build_clean; do
            toclean="$toclean$jagen_S$dir"
        done
    elif [ "$pkg_config" ]; then
        toclean="$pkg_build_dir"
    else
        # This works for dist in_source packages only because the source dir is
        # inside the work dir in the default configuration.
        toclean="$pkg_work_dir${jagen_S}$pkg_source_dir"
    fi
    for dir in $toclean; do
        if [ -d "$dir" ]; then
            if jagen_is_same_dir "$dir" "$pkg_source_dir"; then
                if pkg__is_scm; then
                    pkg_run _jagen src clean "$pkg_name"
                else
                    warning "not removing '$dir' of $pkg_name: it is the source directory"
                fi
            else
                debug "removing $dir"
                pkg_run rm -rf "$dir"
            fi
        fi
    done
}

pkg_unpack() {
    local IFS; unset IFS
    set -- $pkg_source
    local src_type="$1"
    local src_path="$2"

    case $src_type in
        dist|dist:*)
            pkg__unpack_dist "$src_path" "$pkg_work_dir"
            ;;
        git|hg|repo)
            pkg_run _jagen src update "$pkg_name"
            ;;
        dir|'') ;;
        *)
            die "unknown source type: $src_type"
            ;;
    esac
}
