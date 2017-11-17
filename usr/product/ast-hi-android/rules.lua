-- Android rules

package { 'android-cmake' }

package { 'cmake-modules' }

package { 'hi-sdk-tools' }

package { 'android', 'target',
    env = { 'target', 'lunch' },
    { 'patch',
        { 'hi-sdk-tools', 'unpack' }
    }
}
