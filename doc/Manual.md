# User Manual

## Contents

- [Initializing](Initializing.md)
- [List Information](List.md)
- [Building](Building.md)
- [Cleaning](Cleaning.md)
- [Rust Support](Rust.md)
- [Managing package sources](ManagingSources.md)
- [Manage filesystem images](Images.md)
- [Install Bash completions](Installation.md)
- [Rules](Rules.md)
- [Targets](#targets)
- [Build system internals](#build-system-internals)

### Introduction

A workflow is organized around "projects" which are composed from several
layers. Each layer can contribute rule definitions and environment variables to
the project. These definitions are evaluated to generate meta build system
which is then used to track package dependencies, allows selective rebuild,
cleanup and management of VCS sources.

Every rule, environment file or script can be overridden in the subsequent
layers or by the project itself. A library of built-in functions is provided to
ease integration of packages using common build systems like autotools and
CMake with their own set of workarounds. There are several vendor-specific
layers in the Jagen distribution which can be used as an example.

In short, it's a fresh remix of ideas from [Repo][], [BitBake][] and
[Buildroot][] with focus on speed and ease of use. Can be considered a much
simpler and a lighter-weight alternative to [OpenEmbedded][]/[Yocto][].

  [Repo]: https://source.android.com/source/using-repo.html
  [BitBake]: https://en.wikipedia.org/wiki/BitBake
  [Buildroot]: https://buildroot.org
  [Yocto]: https://www.yoctoproject.org/
  [OpenEmbedded]: http://www.openembedded.org

### Targets

```
  Targets are specified as '<name>:<stage>:<config>'. Available package stages
  are filtered with the given expression. Omitted component means 'all'.  For
  example:

  utils              - select all stages of the utils package
  utils:install      - select all utils install stages
  utils::host        - select all host utils stages
  utils:compile:host - select only host utils compile stage
  :build:host        - select host build stages of all packages

  When a target is succesfully built the stamp file is created in the build
  directory with the name: <name>__<stage>__<config>. This file is used to
  determine if the target is up to date. Deleting it will cause the
  corresponding target to be rebuilt unconditionally next time the build system
  runs.
```

## Build system internals

The build system is generated from a set of rules represented by tables
(key-value pairs) which are found in `rules.lua` and `pkg/<name>.lua` files
across layer directories. Each rule defines some piece of information about a
package: build stages, type, location of source directory and so on. Rules with
the same name are considered as belonging to the same package but evaluated
independently at the point of reference. So, mentioning the package in the
"requires" list of a package which has config will produce different result from
standalone "package" declaration. Also order of rules matters, both in the rules
file and across layers. See the `package` function in `src/Package.lua` file for
complete information about evaluation logic.

Package dependencies and build commands are written to `build.ninja` file in
the `build` directory, additional package specific environment goes into
"include scripts" in the `include` directory. All code dealing with include
script generation is in `src/Script.lua` file which can be used as a reference.
Dependencies are resolved by touching specifically named files in the build
directory after the command succeeded, see the generated `build.ninja` for
details.

At the core of the build system is a `jagen-stage` script. Given a package
name, stage and config it finds and imports all necessary environment files,
runs the stage script and returns its result. Default build stage scripts and
utility functions are placed in a `src/pkg.sh` file which can be used as a
reference.  Every `pkg/<name>.lua` file can have `pkg/<name>.sh` backing file
in the same directory which is included by `jagen-stage` during the build and
can be used to override default stages or environment.

Layers, build type and directory locations are set in 'config.sh' which is
generated during project initialization. It is also included indirectly in
every build command.
