### Initializing

Use the `init` script in the root of Jagen source directory to initialize the
current directory as the project. The script should be run reaching out by a
relative path to the checked out Jagen sources or piped to shell. Like so:

    cd ~/src
    git clone https://github.com/bazurbat/jagen.git
    mkdir jagen-project
    cd jagen-project
    ../jagen/init

Or, if you want simpler setup:

    mkdir -p ~/src/jagen-project
    cd ~/src/jagen-project
    curl -fsSL https://raw.githubusercontent.com/bazurbat/jagen/master/init | sh

This will clone the Jagen itself into the `.jagen` subdirectory and then
initializes the project. This way it will be self-contained and can be moved
around, but note that the build system generates files which may contain
absolute paths, so, if you really plan to move the project you should issue
`clean` command before and `refresh` after.

The `init` creates `config.sh`, `env.sh` and `jagen` files inside the project
directory.

The generated `config.sh` contains parameters from the `init` command line
(layers, flags) and other global settings. Edit it manually to adjust the
parameters or global environment. It is sourced every time the build system
runs. 
  
For interactive work source the generated `env.sh` to initialize current Shell
environment for this project from now on. This will put `jagen` command in PATH
and allow Bash to autocomplete its commands and the project's targets. Note
that mixing the environments from different projects will likely produce
unexpected results.

Use the `jagen` script inside the project directory to run Jagen commands for
this project from outside without modifying the environment. This could be
useful to activate the build from IDEs and such.

#### Options

Option        | Description
--------------|------------
-h, --help    | show usage information
-a, --flag    | add the flag to jagen_flags
-i, --include | add the directory to jagen_include_path
-f, --force   | force to initialize non-empty directory

The following flags are reserved by the core:
  
 - `ccache`  — activate the usage of `ccache` for all toolchain commands
 - `offline` — causes all operations requiring the network to fail

Reinitializing an existing project is possible with `--force` but note that it
will regenerate the `config.sh`. The previous is saved as `config.sh.bak`.
Copy your old settings from it manually if necessary.

The list of project layers is constructed from non-option arguments. Each layer
can contribute rule definitions and environment overrides to the project.

The project directory can also be called "build root" for a family of packages.
A layout for a typical build root is described below:

Path     | Description
---------|------------
/bin     | generated helper scripts
/build   | build system working directories
/dist    | the downloaded distribution archives and files
/host    | install root for 'host' configuration
/include | package-specific include scripts generated from rules
/lib     | project-specific rules and pkg scripts
/log     | build log files
/src     | checked out sources for SCM packages
/target  | install root for 'target' configuration

The generated environment binds the project to the corresponding jagen source
directory. If one or the other is moved or sourced from different root from
which it was originally initialized (like chroot or Docker container) any
build-related command will likely produce wrong results.
