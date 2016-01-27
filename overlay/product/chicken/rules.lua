-- CHICKEN Scheme

package { 'chicken', 'host' }

package { 'chicken-eggs', 'host' }

package { 'chicken', 'target',
    { 'build', { 'chicken', 'install', 'host' } }
}

package { 'chicken-eggs', 'target',
    { 'build',
        requires = {
            'dbus',
            'sqlite',
        },
        { 'chicken-eggs', 'install', 'host' }
    }
}
