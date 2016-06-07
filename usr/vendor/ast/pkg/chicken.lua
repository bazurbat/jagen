local pkg = {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/chicken-scheme.git',
        branch   = 'release-cmake'
    },
    build = {
        type = 'CMake'
    }
}

if Jagen.flag('new_chicken') then
    pkg.source.branch = 'master-cmake'
end

return pkg
