return {
    source = {
        type = 'git',
        location = 'https://github.com/google/protobuf.git',
        branch = '3.5.x'
    },
    build = {
        type = 'GNU',
        generate = true
    },
    requires = {
        { 'unzip', 'system' }
    }
}
