return {
    source = {
        type     = 'git',
        location = 'https://github.com/bazurbat/protobuf.git',
        branch   = '3.4.x',
        exclude_submodules = true,
    },
    build = {
        type = 'gnu',
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
                    '--disable-maintainer-mode',
                    '--disable-64bit-solaris',
                    '--enable-shared',
                    '--enable-static',
                    '--without-zlib',
                    '--with-protoc=$jagen_host_dir/bin/protoc'
                }
            }
        }
    }
}
