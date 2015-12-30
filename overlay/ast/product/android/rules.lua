-- AST HiSilicon Android

package { 'hi-kernel' }

package { 'hi-sdk', 'target',
    { 'unpack',
        { 'hi-kernel', 'unpack' }
    }
}

package { 'android',
    { 'unpack',
        { 'hi-sdk', 'unpack' }
    }
}
