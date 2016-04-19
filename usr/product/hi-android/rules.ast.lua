-- AST HiSilicon Android

rule { 'hi-sdk-tools' }

rule { 'android',
    env = { 'target' },
    { 'patch',
        { 'hi-sdk-tools', 'unpack' }
    },
    { 'configure',
        requires = { { 'toolchain', 'target' } }
    }
}
