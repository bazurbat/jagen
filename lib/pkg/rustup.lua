return {
    -- the executable should be named 'rustup-init' or it will not run
    -- complaining that the default toolchain is not set
    source = { 'dist', 'https://static.rust-lang.org/rustup/dist/$jagen_host_arch/rustup-init' },
    build = {
        type    = 'executable',
        system  = '$jagen_host_arch',
        options = {
            '-y',
            '--no-modify-path',
            '--default-host', 'none',
            '--default-toolchain', 'none'
        },
        toolchain = false
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
