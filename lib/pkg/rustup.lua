return {
    -- the executable should be named 'rustup-init' or it will not run
    -- complaining that the default toolchain is not set
    source = 'https://static.rust-lang.org/rustup/dist/x86_64-unknown-linux-gnu/rustup-init',
    build = {
        type = 'executable',
        options = {
            '-y',
            '--no-modify-path',
            '--default-toolchain',
            'none'
        }
    },
    install = {
        type = true,
    },
    env = {
        CARGO_HOME = "$pkg_build_dir"
    },
    export = {
        env = {
            CARGO_HOME = '$pkg_env_CARGO_HOME'
        }
    }
}
