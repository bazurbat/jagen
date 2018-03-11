return {
    source = {
        type      = 'dist',
        location  = 'https://tuxera.com/opensource/ntfs-3g_ntfsprogs-2016.2.22.tgz',
        sha256sum = 'd7b72c05e4b3493e6095be789a760c9f5f2b141812d5b885f3190c98802f1ea0'
    },
    build  = {
        type = 'gnu',
        options = {
            '--disable-shared',
            '--disable-static',
            '--enable-ldscript',
            '--disable-ldconfig',
            '--disable-library',
            '--disable-posix-acls',
            '--disable-ntfsprogs',
            '--without-uuid',
            '--without-hd',
        }
    }
}
