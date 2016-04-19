-- CHICKEN Scheme

rule { 'chicken', 'host' }

rule { 'chicken-eggs', 'host' }

rule { 'chicken', 'target',
    { 'configure', { 'chicken', 'install', 'host' } }
}

rule { 'chicken-eggs', 'target',
    { 'configure',
        requires = {
            'dbus',
            'sqlite',
        },
        { 'chicken-eggs', 'install', 'host' }
    }
}
