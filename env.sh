jagen_relative_root="."

if [ "$1" = relative ]; then
    jagen_root="$jagen_relative_root"
else
    jagen_root="$PWD"
fi

jagen_build_root="$PWD"

export jagen_root jagen_build_root

. "$jagen_root/lib/env.sh" ||
    { echo "Failed to load environment"; return 1; }

add_PATH "$target_bin_dir"
add_PATH "$pkg_private_dir/bin"
add_PATH "$jagen_root/bin"
