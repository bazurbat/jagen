## Build System

### Stages

Each package in the generated build system divided by a number of stages which are performed in
order:

* clean
* update/unpack
* patch
* clean:config
* configure:config
* compile:config
* install:config

Those stages can be subdivided to "common" and "config-specific". Each package has at least the
following common stages: "clean", "update" (for SCM type sources) or "unpack" (for other sources)
and "patch".

If some build type is set (`build.type` is not `nil`) and there are some configs defined for the
package then "configure" and "compile" stages are added. Additionally, `install.type` is set from
`build.type` unless overridden which causes "install" stage to be added.

The "clean" stages return the package to the initial state by removing temporary build directories.
Common stage deletes `$pkg_work_dir` (\<project dir\>/build/\<package name\>) directory by default,
config-specific clean stages remove `$pkg_build_dir` (\<project dir\>/build/\<config\>). The
directories removed by "clean" stages can be changes by setting `build.clean` property.

The "update" stage updates the source repository by pulling or cloning if the target directory is
not exists. The "unpack" stage unpacks the distribution archive to the `$pkg_work_dir`.

The "patch" stages applies patches and copies supplementary files if defined or can be overridden
to perform custom steps to prepare the package for the build.

The "configure" stage runs `configure` for "gnu" build type or CMake for "cmake" build type passing
it the defined build options.

The "compile" runs runs steps necessary for compilation, for most build types this just means
"make" command.

The "install" stage runs `make install` or equivalent for the defined install type.

### Configs

There are 2 predefined package configs in Jagen: "host" and "target". The "host" packages are
installed to the `host` subdirectory of the project root and are generally meant as helpers for
building the "target" packages (such as code generators, debuggers and utilities). The "target"
packages are meant for the target system, so they are cross-compiled using the chosen target
toolchain and are installed to the `target` subdirectory of the project which can also serve as a
staging area.

To allow running "host" packages directly for the current project the `host/bin` is added to
`$PATH` and `host/lib` is added to `$LD_LIBRARY_PATH` when you source the generated `env.sh` script
in the project directory.

Additionally, if no package rules with explicitly set config found but there are definitions
without config an empty package with "host" config is automatically defined for each such rule.
This facilitates the use case when you just need to compile a set of packages for the local system
using the native toolchain.

If the package is built or installed (has non empty `build.type` or `install.type` it will also
always have a config. So, even if you have not written any rules with the config set the "host"
will be added anyway and you will need to spell the stage by full name such as
`package:compile:host` when specifying the targets to build.

### Logs

All output from the build command is saved in `log` subdirectory of the project in files named
after the target with `.log` appended, such as `log/zeromq:patch.log` for patch stage of a package
"zeromq" or `log/zeromq:install:host.log` for "install" stage of the package "zeromq" for the
"host" config.

When you see that some target failed inspect those log files to see the commands output to diagnose
the problem.

### Targets

Targets are specified as '\<name\>:\<stage\>:\<config\>'. Available package stages are filtered
with the given expression. Omitted component means 'all'.  For example:

- utils              - select all stages of the utils package
- utils:install      - select all utils install stages
- utils::host        - select all host utils stages
- utils:compile:host - select only host utils compile stage
- :build:host        - select host build stages of all packages

When a target is successfully built the stamp file is created in the build directory with the name:
`<name>:<stage>:<config>`. This file is used to determine if the target is up to date. Deleting it
will cause the corresponding target to be rebuilt unconditionally the next time the build system
runs.

## Build system internals

_TODO: this section is out of date._

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
