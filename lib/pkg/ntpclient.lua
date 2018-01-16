return {
    source = {
        type      = 'dist',
        location  = 'http://doolittle.icarus.com/ntpclient/ntpclient_2010_365.tar.gz',
        sha256sum = '9ad9b028385082fb804167f464e2db0a0b3d33780acd399327e64898b8fcfddd',
        dir       = 'ntpclient-2010'
    },
    build  = {
        type = 'make',
        in_source = true,
        set_toolchain = true
    }
}
