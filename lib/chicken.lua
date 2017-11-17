-- CHICKEN Scheme

package { 'chicken', 'host' }

package { 'chicken-eggs', 'host' }

package { 'chicken', 'target',
    { 'configure', { 'chicken', 'install', 'host' } }
}

package { 'chicken-eggs', 'target',
    requires = {
        'dbus',
        'sqlite',
    },
    { 'configure',
        { 'chicken-eggs', 'install', 'host' }
    }
}

if Jagen.flag('new_chicken') then
    package { 'chicken', 'host',
        requires = { 'chicken-boot' }
    }
end
