return {
    source = {
        type     = 'git',
        location = 'https://github.com/bazurbat/grpc.git',
        branch   = 'v1.7.x',
    },
    build = {
        type = 'make',
        in_source = true
    },
    install = {
        args = { 'prefix=$pkg_install_dir' }
    },
    requires = {
        'protobuf',
    }
}
