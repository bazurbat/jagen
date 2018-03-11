return {
    source = {
        type     = 'git',
        location = 'https://github.com/google/flatbuffers.git',
        tag      = 'v1.8.0'
    },
    build = {
        type = 'cmake',
        options = {
            '-DFLATBUFFERS_BUILD_TESTS=NO'
        }
    }
}
