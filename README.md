# jagen

Just another build system generator.

- [Overview](#overview)
- [Requirements](#requirements)
- [Usage](#usage)
    - [Initializing](#initializing)
    - [Building](#building)
    - [Cleaning](#cleaning)
    - [Targets](#targets)
    - [Working with source packages](#working-with-source-packages)
    - [Manage filesystem images](#manage-filesystem-images)
    - [Install Bash completions](#install-bash-completions)
- [Build system internals](#build-system-internals)

## Overview

Jagen is a tool which eases development of multiple interdependent software
packages. It abstracts peculiarities of individual packages build systems and
provides means to manage them as a whole.

A workflow is organized around "projects" which are composed from several
layers. Each layer can contribute rule definitions and environment variables to
the project. The definitions are merged to generate meta build system which is
then used to track individual packages build stages, allows selective rebuild
or cleanup and management of sources.

Rules are declarative key-value pairs (dictionaries) defining a piece of
information about a package. Rules with the same name are merged but there can
be a few configurations for the same package inside a single project. This
mechanism is used to support cross-compilation, but it is not limited to that.

Every rule, environment file and build stage can be overridden in the
subsequent layers or by the project. Built-in functions are provided to ease
integrating of packages using common build systems like autotools and CMake
with their own sets of workarounds. There are several vendor-specific layers in
the Jagen distribution which can be used as an example.

## Requirements

POSIX compatible shell, Lua 5.1 or 5.2, [Ninja](https://ninja-build.org/).

## Usage

### Initializing

```
Usage: init-project <CONFIG> [OPTIONS...]
       init-project [-h]
       init-project [-l]

  Initializes current directory as jagen project.

SYNOPSIS:

  The script will put an environment file 'env.sh' and a configuration file
  'config.sh' in the current directory. The environment file should be sourced
  into the working shell before issuing any other jagen commands. The
  configuration file is sourced by a generator and a build system.

  Jagen will create and remove few directories inside the build root depending
  on the selected configuration and commands given, so it is not safe to store
  important data there. Also initializing jagen's own project directory as
  build root is not supported. It is recommended to use separate directory for
  every configuration and do not mix shell environments from different
  projects.

OPTIONS:

  -a  add flag
  -h  show this help
  -l  list config templates
  -s  share sources between projects
  -f  use force

  In the default configuration a location of software distributions, patches
  and toolchains is set relative to a root directory (one level above checked
  out Jagen sources by default) to facilitate sharing between different
  projects. Source packages are checked out into the 'src' subdirectory of the
  current project. Use the '-s' option to set a location of source packages
  relative to the root directory too. Note that 'jagen clean' command does not
  touch the source packages location even if it is inside the build root.

  The command refuses to initialize non-empty directories by default. Use '-f'
  option to override the check.

  The generated environment binds the project to the corresponding jagen source
  directory. If one or the other is moved or sourced from different root from
  which it was originally initialized (like chroot or Docker container) any
  build-related command will likely produce wrong results.

  The generated configuration can be adjusted manually but will be overwritten
  by the next 'init-project' invocation. Use '-a' option to set 'jagen_flags'
  at the initialization time; it can be specified multiple times.

EXAMPLES:

	# assuming jagen is checked out into ~/work
	cd ~/work
    mkdir ast100
    cd ast100
    ../jagen/init-project ast100 -a flag1 -a flag2
    . ./env.sh
    jagen build
    exit

  For subsequent invocations:

    cd ~/work/ast100
    . ./env.sh
    jagen build -f target1 target2

```

### Building

```
Usage: jagen build [OPTION...] [TARGET...]

  Builds or rebuilds the specified targets.

OPTIONS

  -h, --help          print this help message
  -n, --dry-run       print expanded value of TARGET... arguments and exit
  -p, --progress      show TARGET's build progress
  -P, --all-progress  show all build output
  -f, --force         force rebuild of the specified targets
  -a, --all           build everything out of date

  Use command 'jagen help targets' for information about targets.

SYNOPSIS

  If no targets were specified the command builds everything not already built;
  otherwise it expands TARGET... arguments and builds the resulting targets if
  they are out of date. The '--force' option causes the specified targets to be
  rebuilt unconditionally. Use '--all' option to also build dependencies with a
  single command.

  Short options can be combined into a single argument, for example:

    jagen build -fap libuv

  will rebuild all targets of libuv package (starting from 'unpack', see below)
  showing progress on the console. Afterwards it will continue building
  dependent targets until everything is up to date.

  Note that all packages start with 'unpack' stage which removes working
  directories from previous builds, cleans sources from extra files, unpacks
  distributions and updates the sources from scm. It does not touch scm sources
  if there are changes detected (jagen src dirty returns true) but still
  removes build directory if it is separate from source directory. This is
  especially important to remember when working on packages with "in source"
  builds.

  For development and testing it can be more convenient to select specific
  targets, like:

    jagen build -fp libuv:compile:target

  This will recompile libuv for target configuration showing output on the
  console and do nothing else.

```

### Cleaning

```
Usage: jagen clean [package[:config]...]

  Deletes package build directories or all generated files and directories
  inside the current build root.

SYNOPSIS

  There can be multiple arguments in the form of: <name> or <name>:<config>.
  Build directories of given packages will be removed. If <config> is supplied
  the corresponding build directory will be removed, if only <name> is supplied
  all build directories will be removed.

  If no arguments are given, than everything in the current build root is
  cleaned up. The following directories are recursively deleted:

    jagen_build_dir
    jagen_include_dir
    jagen_log_dir
    jagen_host_dir
    jagen_target_dir

  Actual paths depend on configuration. After the deletion regenerates the
  build system using the 'jagen refresh' command.
```

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

### Working with source packages

```
Usage: jagen src <command> [PACKAGES...]

  Manage SCM package sources.

SYNOPSIS

  The optional PACKAGES argument should be the list of SCM packages defined in
  the current environment. If none are specified, all are assumed.

COMMANDS

  dirty   Check if packages source directories have any changes
  status  Show packages location, head commit and dirty status
  clean   Clean up packages source directories
  update  Update the sources to the latest upstream version
  clone   Clone the specified packages
  delete  Delete packages source directories

  The 'dirty' command exits with 0 (true) status if any of the specified
  packages source directories have changes. It exits with 1 (false) status if
  all sources are clean. Intended for usage by shell scripts.

  The 'status' command prints SCM packages status in human readable form.

  The 'clean' command resets modifications to the HEAD state and deletes
  all extra files in packages source directories.

  The 'update' command fetches the latest sources from upstream and tries to
  merge them. It does nothing if there are modifications in the working
  directory (dirty returns true); commit, stash or clean changes before issuing
  the 'update' command in this case.

  The 'clone' command clones the specified packages.

  The 'delete' command deletes packages source directories.
```

### Manage filesystem images

```
Usage: jagen image <command> <options...>

  Manages filesystem images.

COMMANDS

  create  Create images

  The 'create' command creates filesystem images according to configuration.
  Currently the only possible command is:

    jagen image create rootfs

  in 'hi-linux' build root which creates rootfs image.
```

### Install Bash completions

  1. Make sure you are sourcing `bash_completion` in your `.bashrc` (necessary
     in Ubunty, not necessary in Gentoo).

```
     source "/usr/share/bash-completion/bash_completion"
```

  2. Add contents of `<jagen_dir>/misc/bash_completion` to `~/.bash_completion`
     file (or `~/.config/bash_completion` in Gentoo).

Completions defined for `jagen` command will work only when environment is
sourced from build root. Freeform target patterns such as `::host` or
`:install:` are not currently completed.

## Build system internals

The build system is generated from a set of rules represented by dictionaries
(key-value pairs) which are found in "rules.lua" files across "layer"
directories. Each rule defines some piece of information about a "package",
like build stages, location of source directory and so on. Rules with the same
names are merged across layers and the final result is used in generation.
Dependency information and build command goes into `build.ninja` file in the
`build` directory, additional package specific environment goes into "include
script" in the `include` directory. All code dealing with include script
generation is in `src/Script.lua` file which can be used as a reference.

Build stages are handled by `jagen-pkg` which includes `lib/pkg.sh` which
contains definitions of default stages and some utility functions for usage in
user defined build scripts. It is the actual "engine" of the build system.

Layers, build type and directory locations are set in 'config.sh' which is
generated during project initialization. It is also included indirectly in
every build command.
