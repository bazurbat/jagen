# Jagen

[![Join the chat at https://gitter.im/bazurbat/jagen](https://badges.gitter.im/bazurbat/jagen.svg)](https://gitter.im/bazurbat/jagen?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Jagen is a utility tool that saves software engineers time. It combines selected features of
workspace and package managers to automate common day-to-day development and integration tasks such
as setting up the environment, downloading distribution archives and toolchains, managing patches,
keeping source repositories up to date and rebuilding parts of the project as needed.

It is designed to be a very lightweight alternative to
[Repo](https://source.android.com/setup/develop/repo)/[GClient](https://www.chromium.org/developers/how-tos/depottools)
and [Yocto](https://www.yoctoproject.org).

## Introduction

- [Features](#features)
- [Getting Started](#getting-started)
- [Package Rules](#package-rules)
- [Building](#building)

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

## Features

- Simple declarative rules to define packages.
- Initialize a build root using a single command (fast onboarding).
- Reproduce the same environment on CI and development systems.
- Build or rebuild any stage of any package respecting dependencies.
- Complex projects can consist of layers (similar to OE/Yocto but much more straightforward).
- Out of the box support for common build systems: CMake, Autotools, Android Gradle. In many cases
  can handle packages using bare Make as well without additional configuration.
- Built-in source management facilities (similar to Repo and GClient) supporting
  [Git](https://git-scm.com) and [Mercurial](https://www.mercurial-scm.org) (Hg).
- Automatic downloading of distribution archives (from everything curl supports \+ special handling
  of Google Drive).
- Designed for cross-compilation from the start.
- Easy to add custom pre-built toolchains.
- Supports using of multiple toolchains in the same build root.
- Packages can dynamically export the environment or settings for other packages.
- First-class [Rust](https://www.rust-lang.org/) language support.
- Fully assisted Bash completion (fast and offers items relevant to the current project).
- An extensive set of facilities to accommodate packages with custom build systems or special
  needs.

## Getting Started

A directory which groups related source code, distribution archives, build artifacts and other
auxiliary files, is called a "workspace", a "root directory" or a "build root" interchangeably.
Build roots can consist of multiple layers defining a project's environment and components which are
merged together according to rules to form a complete build system.

To start using Jagen create a directory for the workspace and make it current:

```
mkdir jagen-root && cd jagen-root
```

Initialize this directory by piping Jagen's init script into the shell:

```
curl -fsSL https://git.io/fhyEM | sh
```

Create a `rules.lua` file and add your rules there. Use the generated `./jagen` script to run the
build system.

If you already have some layers prepared pass their URLs to the shell after the `--`. For example,
to create a workspace preconfigured with a [tutorial layer](https://github.com/bazurbat/jagen-start)
located at https://github.com/bazurbat/jagen-start use the command:

```
curl -fsSL https://git.io/fhyEM | sh -s -- https://github.com/bazurbat/jagen-start
```

See a list of defined packages:

```
./jagen list packages
```

Run the build system:

```
./jagen build
```

During the build, Jagen will download/clone sources and run packages configure/compile/install
stages in order as needed until everything is done. You can find the results in a `host`
subdirectory which is used as a default staging directory for packages using the system's native
toolchain.

To modify the list of layers or flags or refresh the generated environment for an existing workspace
run the `init` script again with the appropriate arguments inside the workspace's directory. To
guard against unexpected changes to the script in the master branch rerun it from the local copy
which was cloned during the workspace creation:

```
./.jagen/init [OPTIONS...]
```

For more complex configuration changes than just setting flags or layers edit the `config.sh` file
manually. Do not rerun the `init` script in this case because it generates the default `config.sh`.

To download rules from the tutorial layer for experiments:

```
curl -fsSL https://raw.githubusercontent.com/bazurbat/jagen-start/master/rules.lua > rules.lua
```

Edit the `rules.lua` file and rerun the `./jagen build` command to bring the workspace up to date.

## Package Rules

Jagen generates a build system from declarative rules found in `rules.lua` files across the
workspace's layers. An order is significant with the later rules appending and overriding preceding
definitions. The `rules.lua` file in the workspace itself is processed last, so it is a usual place
to define package exclusions and build type (release/debug) specific for the build root. In the
simplest case, the workspace's `rules.lua` can define all the packages as well but for easier
sharing of the resulting environment, it is recommended to put it into a separate Git repository and
use init commands outlined above once the ruleset stabilizes.

For an example, here are the contents of the `rules.lua` from the tutorial layer:

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

The simplest rule has the form:

```lua
package { 'mypackage' }
```

Which defines an empty package named "mypackage". To enable Jagen's source management facilities you
need to specify the source location of the package by setting the `source` property, for example:

```lua
package { 'mypackage',
    source = 'https://github.com/nanomsg/nanomsg.git'
}
```

Run the `jagen source update mypackage` command and find the nanomsg sources in the `src/mypackage`
subdirectory of the build root. So, to use Jagen as a workspace manager it is enough to define a few
packages this way and issue `jagen source update` command periodically to update them. If no other
source properties are set Jagen will clone the default branch ("master" most of the time in the case
of Git). Perhaps more common case, especially for dependent repositories, is a need to clone a
specific tag. This can be done by setting the `tag` property of the source:

```lua
package { 'nanomsg',
    source = { 'https://github.com/nanomsg/nanomsg.git',
        tag = '1.1.4'
    }
}
```

or, alternatively, using the non-shorthand syntax:

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

In addition to the source management, Jagen includes extensive facilities to orchestrate the
building of software. Its role is to prepare an environment, compile and install dependencies and
call a package's own build system with the correct parameters. To enable the build support you need
to specify the type of the build system of the package, like so:

```lua
package { 'nanomsg',
    source = {
        location = 'https://github.com/nanomsg/nanomsg.git',
        tag = '1.1.4'
    },
    build = 'cmake'
}
```

Run `jagen build nanomsg` command to build it. By default, the building of the package also implies
that it needs to be installed to be found by dependent packages. Given the rule above the `nanomsg`
will be installed in the `host` subdirectory of the build root.

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
