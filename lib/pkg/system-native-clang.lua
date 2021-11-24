package {
    name = 'system-native-clang',
    class = 'toolchain',
    source = {
        dir = '/usr'
    },
    install = 'toolchain',
    export = {
        cc     = 'clang',
        cxx    = 'clang++',
        cflags = { '-march=native' }
    }
}
