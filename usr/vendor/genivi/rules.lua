define_rule { 'genivi', 'host',
    requires = {
        'audio-manager',
        'audio-manager-plugins',
        'capicxx-dbus-runtime',
    }
}
