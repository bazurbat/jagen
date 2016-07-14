return {
    source = {
        type     = 'dist',
        location = 'http://downloads.sourceforge.net/project/expat/expat/2.1.0/expat-2.1.0.tar.gz',
        sha1sum  = 'b08197d146930a5543a7b99e871cba3da614f6f0'
    },
    build  = {
        type = 'GNU',
        libs = { 'expat' }
    }
}
