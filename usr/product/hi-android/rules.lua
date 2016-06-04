-- Android rules

Pkg:add { 'android-cmake' }

Pkg:add { 'cmake-modules' }

Pkg:add { 'hi-sdk-tools' }

Pkg:add { 'android', 'target',
    env = { 'target', 'lunch' },
    { 'patch',
        { 'hi-sdk-tools', 'unpack' }
    }
}
