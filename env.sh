jagen_relative_root="."

if [ "$1" = relative ]; then
    jagen_root="$jagen_relative_root"
else
    jagen_root="$PWD"
fi

export jagen_root
export jagen_build_root="$PWD"

. "$jagen_root/lib/env.sh"; sts=$?
if [ $sts != 0 ]; then
    echo "Failed to load jagen environment"
    return $sts
fi

add_PATH "$pkg_private_dir/bin"
add_PATH "$jagen_root/bin"
