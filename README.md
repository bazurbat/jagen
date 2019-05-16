# Jagen

[![Join the chat at https://gitter.im/bazurbat/jagen](https://badges.gitter.im/bazurbat/jagen.svg)](https://gitter.im/bazurbat/jagen?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Jagen is an utility tool that saves software engineers time. It combines selected features of
workspace and package managers to automate common day-to-day development and integration tasks such
as setting up the environment, downloading distribution archives, managing patches, keeping
dependent source repositories up to date and rebuilding parts of the project as needed.

It is mostly intended as a very lightweight alternative to
[Repo](https://source.android.com/setup/develop/repo)/[GClient](https://www.chromium.org/developers/how-tos/depottools)
and [Yocto](https://www.yoctoproject.org).

## Features

- Simple declarative rules to define packages.
- Initialize a build root using a single command (fast onboarding).
- Build or rebuild any stage of any package taking dependencies into the account.
- Reproduce the same environment on CI and development machines.
- Complex projects can consist of layers (similar to OE/Yocto but much more straightforward).
- Out of the box support for common build systems: CMake, Autotools, Android Gradle. In many cases
  can handle packages using bare Make as well without additional configuration.
- Built-in source management facilities (similar to Repo and GClient) supporting
  [Git](https://git-scm.com/) and [Mercurial](https://www.mercurial-scm.org/) (Hg).
- Automatic downloading of distribution archives (from everything [curl](https://curl.haxx.se/)
  supports \+ special handling of GDrive).
- Designed for cross-compilation from the start.
- Easy to add custom pre-built toolchains.
- Supports using of multiple toolchains in the same build root.
- Packages can dynamically export environment or settings for other packages.
- First class [Rust](https://www.rust-lang.org/) language support.
- Fully assisted Bash completion (fast and offers items relevant for the current project).
- An extensive set of facilities to accommodate packages with custom build systems or special needs.

## Getting Started

A separate directory containing source code, build artifacts, downloaded distribution archives as
well as Jagen's own related auxiliary files (such as build logs) is called a "workspace", a "root
directory" or a "build root" interchangeably. Build roots can consist of multiple layers describing
a project's components and environment which are merged together to form a complete build system.

To initialize a new workspace for an existing project using Jagen which has its layers already
prepared pipe Jagen's init script into the shell passing layers paths or URLs as arguments:

```
curl -fsSL https://git.io/fhyEM | sh -s -- [OPTIONS...] [URLs...]
```

For example, to create a workspace preconfigured with the [tutorial
layer](https://github.com/bazurbat/jagen-start) located at https://github.com/bazurbat/jagen-start
use the command:

```
curl -fsSL https://git.io/fhyEM | sh -s -- https://github.com/bazurbat/jagen-start
```

This will create a directory named `jagen-root`, clone Jagen's and tutorial layer's repositories
inside it and generate the build system. You can immediately build it:

```
./jagen-root/jagen build
```

During the build Jagen will take care of downloading/cloning/updating sources and running packages
configure/compile/install stages in order as needed until everything is done. Find the results in
the `jagen-root/host` directory.

To create the workspace with a different name instead of `jagen-root` pass the `-d` option to
`init`:

```
curl -fsSL https://git.io/fhyEM | sh -s -- -d myproject https://github.com/bazurbat/jagen-start
```

For reference and a motivational example for new potential users here is the full contents of a
definition file from the tutorial layer:

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

Hopefully it is easy to figure out what those rules are describing. To give it a try initialize a
new empty workspace and save the code above in a `rules.lua` file inside the workspace directory or
just download it directly from GitHub:

```
curl -fsSL https://git.io/fhyEM | sh -s -- -d jagen-test
cd jagen-test
curl -fsSL https://raw.githubusercontent.com/bazurbat/jagen-start/master/rules.lua > rules.lua
./jagen build
```

Modify the rules as you see fit and rebuild the packages by passing their names to the build
command, for example `nanomsg`:

```
./jagen build nanomsg
```

This will also update its repository if needed, if you are changing the source code instead and just
want run configure/compile/install stages use the form:

```
./jagen build nanomsg::
```

Read further for more information about rules format and building software with Jagen.

## Package Rules

Jagen generates a build system based on a set of rules describing project's packages. Rules are
collected from `rules.lua` files which are found across project's layers if any. To make
project-specific adjustments create `rules.lua` in the project's root directory, e.g.
`jagen-root/rules.lua` from the example above.

The simples rule has the form:

```lua
package { 'mypackage' }
```

Which defines an empty package named "mypackage". To enable Jagen's source management facilities you
need specify the source location of the package by setting the `source` property, for example:

```lua
package { 'mypackage',
    source = 'https://github.com/nanomsg/nanomsg.git'
}
```

Run the `jagen source update mypackage` command and find the nanomsg sources in the `src/mypackage`
subdirectory of the build root. So, to use Jagen as a workspace manager it is enough to define a few
packages this way and issue `jagen source update` command periodically to update them. If no other
source properties are set Jagen will clone the default branch ("master" most of the time in the case
of Git). Perhaps more common case especially for dependent repositories is a need to clone a
specific tag. This can be done by setting the `tag` property of the source:

```lua
package { 'nanomsg',
    source = { 'https://github.com/nanomsg/nanomsg.git',
        tag = '1.1.4'
    }
}
```

or, alternatively, using non-shorthand syntax:

```lua
package { 'nanomsg',
    source = {
        location = 'https://github.com/nanomsg/nanomsg.git',
        tag = '1.1.4'
    }
}
```

When a tag is set Jagen will not update the source after the initial clone because tags are not
supposed to change upstream. You can also set a specific revision in a similar manner:

```lua
package { 'nanomsg',
    source = {
        location = 'https://github.com/nanomsg/nanomsg.git',
        rev = 'ef4123ff70c74b47b66ef066ecf88d1ed3750dc3'
    }
}
```

Note that you need to specify a full hash of the commit. This is also true for Mercurial sources
because revision numbers are repository-specific.

Instead of fixing the source to a single revision you can set a branch:

```lua
package { 'nanomsg',
    source = {
        location = 'https://github.com/nanomsg/nanomsg.git',
        branch = 'ws'
    }
}
```

This way the `jagen source update` command will try to bring the specified branch up to date. This
form is useful for tracking upstream development.

### Building

In addition to source management Jagen includes extensive facilities to orchestrate building of
software. Its role is to prepare an environment, build dependencies and call package's own build
system with the correct parameters. To enable the build support you need to specify the type of the
build system of the package, like so:

```lua
package { 'nanomsg',
    source = {
        location = 'https://github.com/nanomsg/nanomsg.git',
        tag = '1.1.4'
    },
    build = 'cmake'
}
```

Run `jagen build nanomsg` command to build it. By default building the package also implies that it
needs to be installed to be found by dependent packages. Given the rule above the `nanomsg` will be
installed in the `host` subdirectory of the build root.

Package dependencies can be specified with `requires` property:

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
```

Just run `jagen build`. Jagen will clone/update sources and build everything in order as needed.

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
