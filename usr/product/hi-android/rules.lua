-- Android rules

define_rule { 'android-cmake' }

define_rule { 'cmake-modules' }

define_rule { 'hi-sdk-tools' }

define_rule { 'android', 'target',
    env = { 'target', 'lunch' },
    { 'patch',
        { 'hi-sdk-tools', 'unpack' }
    }
}
