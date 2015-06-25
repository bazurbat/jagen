package {
    name = 'ast-files'
}

package {
    name = 'make',
    { 'build',   'host' },
    { 'install', 'host' }
}

package {
    name = 'android-cmake'
}

package {
    name = 'libuv',
    { 'build',   'target' },
    { 'install', 'target',
        { 'firmware', 'unpack' }
    }
}

package {
    name = 'ffmpeg',
    { 'build',   'target',
        { 'ast-files', 'unpack' }
    },
    { 'install', 'target',
        { 'firmware',  'unpack' }
    }
}

package {
    name = 'chicken',
    { 'build',   'host' },
    { 'install', 'host' },
    { 'build',   'target',
        { 'chicken',  'install', 'host' }
    },
    { 'install', 'target',
        { 'firmware', 'unpack'          }
    }
}

package {
    name = 'astindex',
    { 'unpack', { 'karaoke-player', 'unpack' } }
}

package {
    name = 'karaoke-player',
    { 'build', 'target',
        { 'android-cmake', 'unpack'            },
        { 'astindex',      'unpack'            },
        { 'chicken',       'install', 'target' },
        { 'ffmpeg',        'install', 'target' },
        { 'libuv',         'install', 'target' },
    },
    { 'install', 'target' }
}

package {
    name = 'firmware',
    { 'install',
        { 'karaoke-player', 'install', 'target' }
    },
    { 'strip'  },
    { 'deploy' }
}
