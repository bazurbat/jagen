package { 'system-native',
    install = {
        type = 'toolchain'
    },
    export = {
        cflags = '-march=native'
    }
}
