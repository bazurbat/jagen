-- AST HiSilicon Android

package { 'hi-kernel' }

package { 'hi-sample', 'target' }

package { 'hi-sdk', 'target',
    { 'unpack',
        { 'hi-kernel', 'unpack' },
        { 'hi-sample', 'unpack' },
    }
}

package { 'android',
    { 'unpack',
        { 'hi-sdk', 'unpack' }
    }
}
