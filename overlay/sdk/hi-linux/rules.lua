-- HiSilicon Linux SDK

package { 'make', 'host',
    source = 'make-3.81.tar.bz2'
}

package { 'hi-kernel' }

package { 'hi-sdk', 'target',
    { 'build_linux',
        { 'hi-kernel', 'unpack' }
    },
    { 'build_common' },
    { 'build_msp'    },
}
