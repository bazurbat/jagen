return {
    source = {
        type = 'dist',
        location = 'https://capnproto.org/capnproto-c++-0.6.1.tar.gz'
    },
    build = {
        type = 'CMake',
        options = {
            -- Disable unit tests. Also allows independent build for target
            -- because tests require running 'capnp_tool' executable on the
            -- host.
            '-DBUILD_TESTING=NO'
        }
    }
}
