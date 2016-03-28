package {
    final = true,
    source = {
        dir = '$jagen_target_dir$jagen_target_prefix'
    },
    build = {
        in_source = true
    },
    { 'install' }
}
