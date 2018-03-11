return {
    source = {
        type     = 'git',
        location = 'https://github.com/GENIVI/dlt-viewer.git',
        tag      = 'v2.18.0'
    },
    build = {
        type = 'cmake',
        options = {
            '-DCMAKE_CXX_FLAGS=-std=c++11'
        }
    },
    requires = {
        'Qt5Core:system',
        'Qt5Quick:system',
        'Qt5Widgets:system',
        'Qt5SerialPort:system',
    }
}
