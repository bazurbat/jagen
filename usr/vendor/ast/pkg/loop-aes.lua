return {
    source = {
        type      = 'dist',
        location  = 'http://loop-aes.sourceforge.net/loop-AES/loop-AES-v3.7b.tar.bz2',
        sha256sum = 'b6794ca655c8a0b9ac47814264d96d7dab588493e3d2c09357eaad1e3436d648'
    },
    build  = {
        type = 'linux-module',
        options = {
            'LINUX_SOURCE=$KERNEL_SRC',
            'KBUILD_OUTPUT=$KERNEL_SRC',
            'USE_KBUILD=y',
            'RUNDM=n',
            'MODINST=n'
        },
        in_source = true,
    },
    install = {
        module_dirs = { 'tmp-d-kbuild' }
    }
}
