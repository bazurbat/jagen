# Jagen

[![Join the chat at https://gitter.im/bazurbat/jagen](https://badges.gitter.im/bazurbat/jagen.svg)](https://gitter.im/bazurbat/jagen?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Jagen eases the development of multiple interdependent software packages by
generating a meta project which abstracts the peculiarities of their respective
build and source control systems and provides instruments to uniformly manage
them as a whole.

It mainly targets embedded development use cases when things should be done
"your way" instead of shoving the requirements into rigid paradigms of Yocto
and Buildroot. In particular, integrating third-party SDKs, pre-built
toolchains or packages with non-standard build systems intended to be a
straightforward process. The focus is on ease of use and adoption rather than
trying to provide yet another idiosyncratic collection of recipes for every
possible piece of software. On the other hand, vendor and project-specific
recipe libraries and patches are well-supported and adding packages using
mainstream build systems (such as Autotools and CMake) is trivial.

The generated build system is simple to understand and customize. It collects
all commands outputs and allows to selectively repeat any stage (such as
configure or compile) of a single or multiple packages respecting the
dependencies and running to completion of a certain target such as creation of
a filesystem image. It is possible to combine multiple projects or parts into a
single product using the concept of "layers".

The built-in workspace management facilities allow SCM-controlled (Git and
Mercurial) packages to be inspected, updated, cleaned up, etc. using universal
commands. They replicate the most common use cases of Google's Repo and gclient
tools such as cloning and updating of multiple repositories. The sources in the
form of distribution archives are also supported. They will be automatically
downloaded and verified during the build. The source repositories and archives
are cached and can be shared between different projects.

Jagen is a mature project which is used in production since 2014.

## Getting Started

For a quick start using Jagen do the following steps:

1. Create a directory for the project and enter it:
```
mkdir root-jagen && cd root-jagen
```
2. Clone Jagen repository and initialize the project:
```
curl -fsSL https://raw.githubusercontent.com/bazurbat/jagen/master/init | sh
```
3. Add the following to the `rules.lua` file in the current directory:
```lua
package { 'nanomsg', 'host',
    source = 'https://github.com/nanomsg/nanomsg.git',
    build  = 'cmake'
}
```
4. Build it:
```
./jagen build
```

The nanomsg is just an example of something small and quick to compile for
demonstration purposes.

During the build the environment will be initialized, the nanomsg sources will
be cloned to the `src` subdirectory, compiled using the default "host"
toolchain and installed to the `host` subdirectory. You are now all set.

Do not forget to add `jagen/misc/bash_completion` to your Bash completion
configuration to make overall experience much more pleasant.

See [User Guide](doc/UserGuide.md) for more information.

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
