## Initializing

Use the `init` script in the root of Jagen repository to initialize current directory as a project.
You can pipe it to Shell directly from GitHub:

    mkdir -p ~/src/jagen-project
    cd ~/src/jagen-project
    curl -fsSL https://raw.githubusercontent.com/bazurbat/jagen/master/init | sh

This will clone Jagen sources to `.jagen` subdirectory and initialize `~/src/jagen-project`
directory as the project. To pass parameters to `init` when piping to Shell use the form:

    curl ... | sh -s -- param1 param2

Non option arguments will be added to `$jagen_layers`, URL arguments will be cloned to `layer`
subdirectory and the resulting directory will be added to the layers list. For example:

    curl -fsSL https://raw.githubusercontent.com/bazurbat/jagen/master/init | sh -s -- -a ccache \
        https://github.com/bazurbat/jagen-lib.git

Will initialize the project with `ccache` flag and `./layer/jagen-lib` layer cloned from
`https://github.com/bazurbat/jagen-lib.git` in a single command. Use this form for quick
deployment.

Alternatively, if you want to share Jagen repository between different projects, you can clone it
separately and run the `init` script reaching out by relative path to the checked out Jagen source
directory. Like so:

    cd ~/src
    git clone https://github.com/bazurbat/jagen.git
    mkdir jagen-project
    cd jagen-project
    ../jagen/init

The project directory can also be called "build root" for a family of packages.

Having shared Jagen sources allows you to build several projects using the same Jagen version which
can be more convenient if you have a lot of projects or are following the master closely, while
piping produces more self-contained project and makes it easier to move it around.

### Options

Option      | Description
------------|------------
-h, --help  | show usage information
-a, --flag  | add the flag to jagen_flags
-f, --force | force to initialize non-empty directory
-L          | add the directory to jagen_layer_path

Flags change Jagen behaviour globally per project. The following flags are reserved by the core:
  
 - `ccache`  — activate the usage of `ccache` for all C/C++ toolchain commands
 - `offline` — causes all operations requiring the network to fail

Reinitializing an existing project is possible with `--force` option. It will regenerate the
`config.sh` saving the previous as `config.sh.bak`. Copy your old settings from it manually if
necessary.

All non-option arguments are considered to be paths to layers which contribute rule definitions and
environment overrides to the project. Absolute paths are used as is, relative paths are resolved
against the project directory, unqualified paths (not starting from `/`, `./` or `../`) are tried
with each entry from `$jagen_layer_path` in turn. If an argument is an URL it will be cloned using
Git to the `./layer` subdirectory inside the project.

### Description

The `init` script creates `config.sh`, `env.sh` and `jagen` files inside the project directory.

The `config.sh` file contains parameters from the `init` command line (layers, flags) and other
global settings. Edit it manually to adjust the parameters or global environment. It is sourced
every time the build system runs. 
  
Source the `env.sh` script to initialize current Shell environment to work with the current project
from now on. This will make the following changes:

- Add Jagen's bin directory to `$PATH` so you can run `jagen` command from anywhere.
- Add project's bin directory to `$PATH` to make generated toolchain wrapper scripts available.
- Add `host/bin` to `$PATH` and `host/lib` to `$LD_LIBRARY_PATH` so you can use packages installed
  with "host" config.
- Activate Bash completions for `jagen` command if installed.

This is recommended mode for interactive work. Note that mixing the environments from different
projects is not supported and will likely produce unexpected results.

Use the `jagen` script inside the project directory to run Jagen commands for this project from
outside without modifying the environment. This is useful for one shot tasks such as building from
an IDE or CI.

A layout for a typical project is the following:

Path     | Description
---------|------------
/bin     | generated helper scripts
/build   | build system working directories
/dist    | the downloaded distribution archives and files
/host    | install root for 'host' configuration
/include | package-specific include scripts generated from rules
/layer   | project-specific layers cloned by `init`
/lib     | project-specific rules and pkg scripts
/log     | build log files
/src     | checked out sources for SCM packages
/target  | install root for 'target' configuration

The generated environment binds the project to the corresponding jagen source directory. If one or
the other is moved or sourced from different root from which it was originally initialized (like
chroot or Docker container) any build-related command will likely produce wrong results.
