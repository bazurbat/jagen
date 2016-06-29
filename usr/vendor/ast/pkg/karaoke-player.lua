return {
    source = {
        type     = 'hg',
        location = 'ssh://hg@bitbucket.org/art-system/karaoke-player',
        branch   = 'master'
    },
    build = {
        type = 'CMake'
    },
    install = {
        dbus_system_configs = {
            'source/agent-smith/org.artsystem.agent.conf',
            'source/fpservice/org.artsystem.fpservice.conf',
        },
        dbus_system_services = {
            'source/agent-smith/org.artsystem.agent.service',
            'source/fpservice/org.artsystem.fpservice.service',
        },
    }
}
