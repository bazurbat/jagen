return {
    source = {
        dir = '/usr'
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
