-- AST HiSilicon Android

package { 'hi-sdk', 'target' }

package { 'hi-sdk-tools' }

package { 'hi-kernel' }

package { 'hi-sample', 'target' }

package { 'android',
    env = { 'target' },
    { 'unpack',
        { 'hi-sdk', 'unpack' }
    },
    { 'configure',
        requires = { { 'toolchain', 'target' } }
    }
}
