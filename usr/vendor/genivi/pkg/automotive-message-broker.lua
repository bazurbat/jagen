return {
    source = {
        type = 'git',
        location = 'https://github.com/otcshare/automotive-message-broker.git',
        branch = '0.13',
    },
    build = {
        type = 'CMake',
        options = {
            '-Denable_icecc=OFF'
        }
    },
    -- using host system for now
    -- also requires uuid lib and headers
    -- requires = {
    --     'boost',
    --     'glib',
    --     'json-c',
    -- }
}
