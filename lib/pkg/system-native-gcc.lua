package {
    name = 'system-native-gcc',
    class = 'toolchain',
    source = {
        dir = '/usr'
    },
    install = 'toolchain',
    export = {
        cc  = 'gcc',
        cxx = 'g++',
        ld  = 'ld',
        cflags = { '-march=native' }
    }
}
