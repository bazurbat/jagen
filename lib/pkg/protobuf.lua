return {
    source = {
        type     = 'git',
        location = 'https://github.com/google/protobuf.git',
        -- "stable" version, newer have problems with cross-compiling
        branch   = '3.1.x'
    },
    build = {
        type = 'GNU',
        generate = true,
        target_requires_host = true,
        options = {
            '--disable-maintainer-mode',
            '--disable-64bit-solaris',
            '--enable-shared',
            '--enable-static',
            '--without-zlib',
        }
    },
    requires = {
        { 'unzip', 'system' },
    },
    configs = {
        target = {
            build = {
                options = {
                    '--with-protoc=$jagen_host_dir/bin/protoc'
                }
            }
        }
    }
}
