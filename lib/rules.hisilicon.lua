package { 'make', 'host',
    source = 'make-3.81.tar.bz2'
}

package { 'hisilicon', 'host',
    { 'build',
        needs = { 'make' }
    }
}
