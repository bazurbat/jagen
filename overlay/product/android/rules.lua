-- Common Android rules

rule { 'make', 'host',
    source = 'make-3.81.tar.bz2'
}

rule { 'android',
    env = { 'lunch' },
    { 'configure',
        requires = { { 'make', 'host' } }
    },
    { 'compile' }
}
