-- CHICKEN Scheme

local Pkg = require 'Pkg'

Pkg:add { 'chicken', 'host' }

Pkg:add { 'chicken-eggs', 'host' }

Pkg:add { 'chicken', 'target',
    { 'configure', { 'chicken', 'install', 'host' } }
}

Pkg:add { 'chicken-eggs', 'target',
    requires = {
        'dbus',
        'sqlite',
    },
    { 'configure',
        { 'chicken-eggs', 'install', 'host' }
    }
}
