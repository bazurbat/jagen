-- Common Android rules

package { 'make', 'host',
    source = 'make-3.81.tar.bz2'
}

package { 'android',
    env = { 'lunch' },
    { 'configure',
        requires = { { 'make', 'host' } }
    },
    { 'compile' }
}
