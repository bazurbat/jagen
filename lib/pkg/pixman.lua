package {
    source = 'pixman-0.32.8.tar.bz2',
    build = {
        type    = 'GNU',
        options = {
			'--disable-dependency-tracking',
			'--disable-static',
			'--disable-mips-dspr2',
			'--disable-gtk',
			'--disable-libpng'
        },
		libs = { 'pixman-1' }
    }
}
