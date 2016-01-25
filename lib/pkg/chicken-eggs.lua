package {
    name   = 'chicken-eggs',
    source = {
        type     = 'git',
        location = 'https://github.com/bazurbat/chicken-eggs.git',
        branch   = 'master'
    },
    { 'install', requires = { 'chicken' } }
}
