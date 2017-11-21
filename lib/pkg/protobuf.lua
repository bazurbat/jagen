return {
    source = {
        type = 'git',
        location = 'https://github.com/google/protobuf.git',
        branch = '3.5.x'
    },
    build = {
        type = 'GNU',
        generate = true,
        options = {
            'DIST_LANG=cpp',
            '--disable-maintainer-mode',
            '--disable-64bit-solaris',
            '--enable-shared',
            '--enable-static',
            '--without-zlib',
        }
    },
    requires = {
        { 'unzip', 'system' },
    }
}
