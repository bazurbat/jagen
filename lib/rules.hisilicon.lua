-- HiSilicon SDK

package { 'make', 'host',
    source = 'make-3.81.tar.bz2'
}

package { 'hisilicon',
    { 'build',
        { 'make', 'build', 'host' }
    }
}
