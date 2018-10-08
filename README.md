# Jagen

[![Join the chat at https://gitter.im/bazurbat/jagen](https://badges.gitter.im/bazurbat/jagen.svg)](https://gitter.im/bazurbat/jagen?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Jagen eases the development of multiple interdependent software packages by generating a meta
project which abstracts the peculiarities of their respective build and source control systems and
provides instruments to uniformly manage them as a whole.

## Getting Started

The fastest way to create a project from scratch is to pipe the init script directly from GitHub.

```
curl -fsSL https://raw.githubusercontent.com/bazurbat/jagen/master/init | sh -s -- -d root-hello
```

This will initialize the `root-hello` directory as an empty project. Add to it a file `rules.lua`
with the following contents:

```lua
package { 'nanomsg',
    source = {
        location = 'https://github.com/nanomsg/nanomsg.git',
        tag = '1.1.4'
    },
    build = 'cmake'

}

package { 'googletest',
    source = {
        location = 'https://github.com/google/googletest.git',
        branch = 'v1.8.x'
    },
    build = 'cmake'
}

package { 'hello-nanomsg',
    source = 'https://github.com/bazurbat/hello-nanomsg.git',
    build = 'cmake',
    requires = { 'nanomsg', 'googletest' }
}

package { 'sqlite',
    source = {
        location = 'https://www.sqlite.org/2018/sqlite-autoconf-3250200.tar.gz',
        sha1sum = 'aedfbdc14eb700099434d6a743135743cff47393'
    },
    build = {
        type = 'gnu',
        options = {
            '--disable-editline',
            '--disable-threadsafe',
            '--disable-dynamic-extensions',
            '--disable-fts4',
            '--disable-fts5',
            '--disable-json1',
            '--disable-rtree',
            '--disable-static-shell'
        }
    }
}

package { 'hello-sqlite',
    source = 'https://github.com/bazurbat/hello-sqlite.git',
    build = 'cmake',
    requires = 'sqlite'
}
```

Run the build command:

```
./root-hello/jagen build
```

During the build Jagen will clone the specified tags or branches of nanomsg, googletest,
hello-nanomsg and hello-sqlite repositories to the `root-hello/src` directory. Also it will
download `sqlite-autoconf-3250200.tar.gz` archive to `root-hello/dist` directory and verify its
checksum. Then it will compile the packages respecting the dependencies as specified by the
`requires` declarations. The compiled packages will be installed to `root-hello/host` directory.
The build system takes this into an account when passing the environment to "configure" stages to
allow CMake and PkgConfig to find their config and module files.

## Reference

- [Initialization](doc/Init.md)
- [Build](doc/Build.md)
- [Clean](doc/Clean.md)
- [Update](doc/Update.md)
- [List Information](doc/List.md)
- [Managing Sources](doc/Source.md)
- [Filesystem images](doc/Image.md)
- [Rust Support](doc/Rust.md)
- [Rules](doc/Rules.md)
- [Build System](doc/BuildSystem.md)
- [Bash completions](doc/Completions.md)

There is also work in progress [User Guide](doc/UserGuide.md).
