-- AST HiSilicon Android

package { 'hi-sdk-tools' }

package { 'android',
    env = { 'target' },
    { 'patch',
        { 'hi-sdk-tools', 'unpack' }
    },
    { 'configure',
        requires = { { 'toolchain', 'target' } }
    }
}
