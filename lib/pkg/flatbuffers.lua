return {
    source = {
        type = 'git',
        location = 'https://github.com/google/flatbuffers.git',
        branch = 'v1.8.0'
    },
    build = {
        type = 'CMake',
        options = {
            '-DFLATBUFFERS_BUILD_TESTS=NO'
        }
    }
}
