-- CHICKEN Scheme

local Pkg = require 'Pkg'

Pkg:add { 'chicken', 'host' }

Pkg:add { 'chicken-eggs', 'host' }

Pkg:add { 'chicken', 'target',
    { 'configure', { 'chicken', 'install', 'host' } }
}

Pkg:add { 'chicken-eggs', 'target',
    { 'configure',
        requires = {
            'dbus',
            'sqlite',
        },
        { 'chicken-eggs', 'install', 'host' }
    }
}
