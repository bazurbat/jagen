package { 'libuv', 'target',
    build  = {
        options = '--disable-static'
    },
    { 'build'   },
    { 'install' }
}

package { 'firmware', 'target',
    { 'install' },
    { 'strip' },
    { 'deploy' }
}
