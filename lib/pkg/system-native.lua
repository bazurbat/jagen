package { 'system-native',
    source = {
        dir = '/usr'
    },
    install = {
        type = 'toolchain'
    },
    export = {
        cflags = '-march=native'
    }
}
