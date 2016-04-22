-- HiSilicon Linux SDK

local Pkg = require 'Pkg'

require 'chicken'

Pkg:add { 'ast-files' }

Pkg:add { 'hi-kernel' }

Pkg:add { 'hi-sdk', 'target',
    { 'patch',
        { 'hi-kernel',    'unpack' },
    },
    { 'prepare', requires = { 'toolchain' } },
    { 'hiboot'    },
    { 'linux'     },
    { 'common'    },
    { 'msp'       },
    { 'component' },
    { 'mkload'    },
}

Pkg:add { 'hi-drivers', 'target',
    { 'configure',
        { 'hi-sdk', 'linux', 'target' }
    }
}

Pkg:add { 'rtl8188eu', 'target',
    { 'compile',
        { 'hi-sdk', 'linux', 'target' }
    }
}

Pkg:add { 'cmake-modules' }

Pkg:add { 'hi-utils', 'target',
    { 'configure',
        { 'cmake-modules', 'unpack'              },
        { 'hi-sdk',        'component', 'target' }
    }
}

Pkg:add { 'karaoke-player', 'target',
    { 'configure',
        requires = {
            'chicken-eggs',
            'connman',
            'ffmpeg',
            'libass',
            'libuv',
            'soundtouch',
        },
        { 'cmake-modules', 'unpack'              },
        { 'hi-sdk',        'component', 'target' },
    }
}

Pkg:add { 'rootfs', 'target',
    { 'install',
        requires = {
            'busybox',
            'dropbear',
            'hi-drivers',
            'hi-utils',
            'karaoke-player',
            'rtl8188eu',
            'wpa_supplicant',
        },
        { 'ast-files', 'unpack'           },
        { 'hi-sdk',    'mkload', 'target' }
    },
}

if jagen.flag 'debug' then
    Pkg:add { 'strace', 'target' }
end
