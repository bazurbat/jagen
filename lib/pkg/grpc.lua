return {
    source = {
        type     = 'git',
        location = 'https://github.com/bazurbat/grpc.git',
        tag      = 'v1.6.x',
    },
    build = {
        type = 'make'
    },
    install = {
        args = { 'prefix=$pkg_install_dir' }
    },
    requires = {
        'protobuf'
    }
}
