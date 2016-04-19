-- CHICKEN Scheme

local R = require 'rules'

R:add { 'chicken', 'host' }

R:add { 'chicken-eggs', 'host' }

R:add { 'chicken', 'target',
    { 'configure', { 'chicken', 'install', 'host' } }
}

R:add { 'chicken-eggs', 'target',
    { 'configure',
        requires = {
            'dbus',
            'sqlite',
        },
        { 'chicken-eggs', 'install', 'host' }
    }
}
