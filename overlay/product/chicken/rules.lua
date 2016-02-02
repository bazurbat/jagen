-- CHICKEN Scheme

package { 'chicken', 'host' }

package { 'chicken-eggs', 'host' }

package { 'chicken', 'target',
    { 'configure', { 'chicken', 'install', 'host' } }
}

package { 'chicken-eggs', 'target',
    { 'configure',
        requires = {
            'dbus',
            'sqlite',
        },
        { 'chicken-eggs', 'install', 'host' }
    }
}
