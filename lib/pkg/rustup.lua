return {
    -- the executable should be named 'rustup-init' or it will not run
    -- complaining that the default toolchain is not set
    source = 'https://static.rust-lang.org/rustup/dist/x86_64-unknown-linux-gnu/rustup-init',
    build = {
        type    = 'executable',
        system  = 'x86_64-unknown-linux-gnu',
        options = {
            '-y',
            '--no-modify-path',
            '--default-host', '$pkg_build_system',
            '--default-toolchain', 'stable'
        }
    },
    install = true,
    env = {
        RUSTUP_HOME = '$jagen_dist_dir/rustup',
        CARGO_HOME = "$pkg_build_dir"
    },
    export = {
        env = {
            RUSTUP_HOME = '$pkg_env_RUSTUP_HOME',
            CARGO_HOME = '$pkg_env_CARGO_HOME',
        }
    }
}
