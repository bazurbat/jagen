return {
    source = {
        -- FIXME: tried to remove on clean
        -- dir = '/usr'
    },
    build = {
        cflags = '-march=native',
        in_source = true,
        toolchain = false
    },
    install = {
        type = 'toolchain'
    }
}
