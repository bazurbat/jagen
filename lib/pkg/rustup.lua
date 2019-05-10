return {
    -- the executable should be named 'rustup-init' or it will not run
    -- complaining that the default toolchain is not set
    source = {
        type = 'dist',
        location = 'https://static.rust-lang.org/rustup/dist/$(jagen_get_system)/rustup-init',
    },
    build = {
        type    = 'executable',
        system  = '$(jagen_get_system)',
        options = {
            '-y',
            '--no-modify-path',
            '--default-host', '$pkg_build_system',
            '--default-toolchain', 'none'
        },
        toolchain = false
    },
    install = true,
    env = {
        RUSTUP_HOME = '$jagen_dist_dir/rustup',
        CARGO_HOME = '$jagen_dist_dir/cargo'
    },
    export = {
        env = {
            RUSTUP_HOME = '$pkg_env_RUSTUP_HOME',
            CARGO_HOME = '$pkg_env_CARGO_HOME'
        }
    }
}
