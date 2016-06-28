# jagen

A straightforward build system generator.

Aimed for cases when OpenEmbedded is too magical and hard to configure for
obscure vendor SDK of choice. Intended to ease the pain of development of
multiple interdependent software packages with non standard toolchains,
cross-compilation and complex build dependencies. Based on ideas from Gentoo
Portage, GNU Guix and Nix package managers.

In contrast to [Buildroot](https://buildroot.org/) the workflow is organized
around development of the rootfs/firmware itself rather than just building it
from distribution packages. There are commands available to rebuild/cleanup
only parts of the tree or only specific targets with dependency tracking. It is
possible to have separate build roots with different configurations/toolchains
building from the same sources. The sources are kept checked out and out of
tree build is setup by default, so there are no intermediate archive/rsync
step.

Built-in functions are provided to ease integrating of packages using common
build systems like autotools and CMake with their own sets of workarounds. It
is trivial to override any built-in stages with local customizations. It is
also easy to hook up foreign build systems facilitating integration of
proprietary packages.

The concept of vendor layers is supported. It is possible to override
everything provided upstream, add your own toolchains, SDKs, BSPs, etc. Every
build root also can have local customizations.

The build system itself is generated from layered set of declarative rules
represented as Lua tables. You can also run arbitrary Lua code during
generation to provide the rules.

## Requirements

POSIX compatible shell, Lua 5.1, [Ninja](https://ninja-build.org/).

## Usage

### Initializing

```
Usage: init-root <CONFIG> [OPTIONS...]
       init-root [-h]
       init-root [-l]

  Initializes current directory as jagen build root.

SYNOPSIS:

  The script will put an environment file '$env_file' and a configuration file
  '$config_file' in the current directory. The environment file should be
  sourced into the working shell before issuing any other jagen commands. The
  configuration file is sourced by a generator and a build system.

  Jagen will create and remove few directories inside the build root depending
  on the selected configuration and commands given, so it is not safe to store
  important data there. Also initializing jagen's own project directory as
  build root is not supported. It is recommended to use separate directory for
  every configuration and do not mix shell environments from different build
  roots.

OPTIONS:

  -a  add flag
  -h  show this help
  -l  list config templates
  -s  share sources between build roots

  In the default configuration a location of software distributions, patches
  and toolchains is set relative to a base directory (\$jagen_dir/..) to
  facilitate sharing between different build roots. Source packages are checked
  out into the current build root (\$jagen_root/src). Use the '-s' option to
  set a location of source packages relative to the base directory too. Note
  that 'jagen clean' command does not touch the source packages location even
  if it is inside the build root.

  The generated environment binds the build root to the corresponding jagen
  project directory. If one or the other is moved it will become invalid. Use
  'init-root' again to reinitialize.

  The generated configuration can be adjusted manually but will be overwritten
  by the next 'init-root' invocation. Use '-a' option to set 'jagen_flags' from
  command line; it can be specified multiple times.

EXAMPLES:

    mkdir -p ~/work/root-ast100
    cd ~/work/root-ast100
    "<jagen_dir>/init-root" ast100 -a flag1 -a flag2
    . ./$env_file
    jagen build
    exit

  For subsequent invocations:

    cd ~/work/root-ast100
    . ./$env_file
    jagen rebuild target1 target2
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
  -f, --from          rebuild starting from the specified targets
  -o, --only          build only matching targets

  Use command 'jagen help targets' for information about targets.

SYNOPSIS

  If no targets were specified the command builds everything not already built;
  otherwise it expands TARGET... arguments and builds the resulting targets if
  they are out of date. The '--from' option causes the specified targets to be
  rebuilt unconditionally following by their dependencies until everything is
  up to date, use '--only' option to skip rebuilding dependencies.

  Short options can be combined into a single argument, for example:

    jagen build -fop libuv

  will rebuild libuv package from scratch, but nothing else, showing progress
  on console. This will make targets depending on libuv out of date, so the
  next 'jagen build' invocation will rebuild them too.

  For development and testing it can be more convenient to select specific
  targets, like:

    jagen build -fp libuv:compile:target

  This will recompile libuv for target configuration if needed, then reinstall
  it to rootfs or firmware image according to the rules currently in effect.
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
  merge them with the current source directories.

  The 'clone' command clones the specified packages.

  The 'delete' command deletes packages source directories.
```

### Install Bash completions

  1. Make sure you are sourcing `bash_completion` in your `.bashrc` (necessary
     in Ubunty, not necessary in Gentoo).

     source "/usr/share/bash-completion/bash_completion"

  2. Add contents of `<jagen_dir>/misc/bash_completion` to `~/.bash_completion`
     file (or `~/.config/bash_completion` in Gentoo).

Completions are defined to `jagen` command only, e.g. work only when
environment is sourced from build root.
