# User Manual

## Contents

- [Initializing](Initializing.md)
- [List Information](List.md)
- [Building](Building.md)
- [Cleaning](Cleaning.md)
- [Targets](#targets)
- [Managing package sources](ManagingSources.md)
- [Manage filesystem images](Images.md)
- [Install Bash completions](Installation.md)
- [Writing build scripts](#writing-build-scripts)
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

## Writing build scripts

### Rules

Rules are processed by the `package` function and should be Lua table of
the following form:

```lua
package { 'name', '[config]',
  <overrides>,
  { 'stage1', '[config]' },
  { 'stage2', '[config]' },
  ...
}
```

where `name` the rule identifier which sets a package name, used to merge the
rules and also to find pkg files. The `config` is optional.

```lua
{
    name = 'package name',
    config = 'package config'
    source = {
        type     = 'dist|git|hg|repo',
        location = 'filename|URL',
        branch   = 'branch|tag|revision',

        md5sum    = 'hash string',
        sha1sum   = 'hash string',
        sha256sum = 'hash string',

        dir      = 'path',     -- variable references are allowed
        basename = 'filename', -- derived from filename if not set
        filename = 'filename', -- derived from location if not set
        exclude  = true,       -- assumed to be 'false' if not set

        ignore_dirty = false
    },
    patches = {
        provider = 'patches',
        { 'filename1', num },
        { 'filename2', num },
        ...
    },
    build = {
        type = 'GNU|CMake|KBuild|make|linux_kernel|linux_module',

        autoreconf = true,
        generate   = true,
        generator  = 'Ninja',
        configure_file = 'path',
        configure_needs_install_dir = true,

        options = { 'option1', 'option2', ... },
        kernel_modules = true,

        in_source = true,
        dir       = 'path',
        profile   = 'release|debug|release_with_debug',
    },
    install = {
        type    = 'GNU|CMake|make|linux_kernel|linux_module|none',

        prefix  = 'path',
        root    = 'path',

        config_script = 'filename',
        libs    = { 'name1', 'name2', ... },
        module_dirs = { 'name1', 'name2', ... },
    },
    requires = {
        'name1',
        'name2',
        ...
    },
    work_dir = 'path'
}
```

- **pkg.name** (`pkg_name`) — the name of the package, it is derived from the
  rule and do not need to be set explicitly

- **pkg.config** — a config for the current pkg rule, also derived from the
  rule or can be set by a template

- **pkg.contexts** — a list of contexts where rules for this package were
  evaluated

- **pkg.source** (`pkg_source_*`) — Source object

- **pkg.patches** — A list of objects of the form:

        { 'name', num }

  where `'name'` is the filename of the patch without the `.patch` extension
  and the `num` is the number of leading slashes to strip from filenames when
  applying the patch (passed as `-pnum` to the `patch` utility).

- **pkg.patches.provider** — optional name of the package providing the
  patches; defaults to "patches" if not specified

- **pkg.build** (`pkg_build_*`) — Parameters for the build stage

- **pkg.install** (`pkg_install_*`) — Parameters for the install stage

- **pkg.requires** — A list of dependencies

- **pkg.template** — A template which should be used for the current rule.
  Essentially this means that instead of the default base rule the specified
  template will be taken and then current rule will be merged into it. This
  template is also passed to the derived rules (from the "requires" lists).
  Can be 'false' to disable applying the template to the current rule.

- **pkg.pass_template** — A template which is passed to the derived rules but
  not applied to the current rule itself. It is useful to break cycles of
  requires.

- **pkg.stages** — Stores rule targets. _Internal._

- **pkg.configs** — Stores config-specific rules and targets. _Internal._

- **pkg.work_dir** (`pkg_work_dir`) — The working directly for the package.
  Default value: `$jagen_build_dir/$pkg_name`.

### Source

- **source.base_dir** — The base directory for the source directly. Defaults to
  `$jagen_src_dir` for SCM sources and `$jagen_build_dir/basename` for dist
  sources.

- **source.basename** (`pkg_source_basename`) — The source filename without an
  extension, `.git` is also considered an extension in this case.

- **source.branch** (`pkg_source_branch`) — The source branch, tag or revision
  to checkout on _unpack_ stage (for SCM source only).

- **source.dir** (`pkg_source_dir`) — A directory name or path of the source
  relative to the `base_dir`.

- **source.exclude** (`pkg_source_exclude`) — If set to `true`, indicates to
  the `unpack` stage to skip the source directly of this package.

- **source.exclude_submodules** — Set to `true` to completely ignore submodules
  found in the Git repository, i.e. do not initialize nor update them
  automatically when switching branches.

- **source.filename** (`pkg_source_filename`) — The last part of the source
  location.

- **source.ignore\_dirty** — Ignore "dirty" status of the source directory.

- **source.location** — The location of the source file or the repository URL.

- **source.md5sum** (`pkg_source_md5sum`) — MD5 hash of the source file (for
  dist sources only)

- **source.sha1sum** (`pkg_source_sha1sum`) — SHA1 hash of the source file (for
  dist sources only)

- **source.sha256sum** (`pkg_source_sha256sum`) — SHA256 hash of the source
  file (for dist sources only)

- **source.type** — A type of the source: dist, git, hg, repo. The packages
  with the source types of "git", "hg" and "repo" are considered "SCM" packages
  and can be managed by the `jagen src` command.

### Build

- **build.arch** — Specifies the target machine architecture. It does not set
  compiler flags by itself but can be used in pkg scripts to do so. For
  packages with the "toolchain" install type it is exported automatically.

- **build.autoreconf** — If set to `true`, indicates that `autoreconf` stage is
  necessary for the package. Also causes `libtool` to be installed for the
  host.

- **build.unset_cflags** (`pkg_build_clean_cflags`) — If set to `true`, causes
  the CFLAGS, CXXFLAGS and LDFLAGS variables to be unset overriding
  config-specific and user environment. Some "SDK-type" packages are very
  sensitive to compiler settings and using this option might be necessary for
  them to build correctly or as a safety measure.

- **build.cflags** — Specifies the C compiler flags. It does not set
  environment variables by itself but intended to be used scripts. For the
  packages with the "toolchain" install type it is exported automatically as
  `CFLAGS`.

- **build.cmake_module_path** (`pkg_build_cmake_module_path`) — Defines
  `CMAKE_MODULE_PATH` for the package. Defaults to `$jagen_cmake_module_path`
  if unset, empty string value disables.

- **build.cmake_toolchain_file** (`pkg_build_cmake_toolchain_file`) — specifies
  `CMAKE_TOOLCHAIN_FILE` for this package only. Set to empty string to disable
  passing of the toolchain file from the environment.

- **build.configure_file** (`pkg_build_configure_file`) — The location of the
  `configure` script (`$pkg_source_dir/configure`).

- **build.configure_needs_install_dir** (`pkg_build_configure_needs_install_dir`) —
  if set, specifies that the `configure` requires libraries and include
  directories from the package install directory to find dependencies;
  effectively this setting adds `-I$pkg_install_dir/include` to `CFLAGS` and
  `-L$pkg_install_dir/lib` to `LDFLAGS`.

- **build.cpu** — Specifies the target machine CPU. It does not set compiler
  flags by itself but can be used in pkg scripts to do so. For packages with
  the "toolchain" install type it is exported automatically.

- **build.cxxflags** — Specifies the C++ compiler flags. It does not set
  environment variables by itself but intended to be used scripts. For the
  packages with the "toolchain" install type it is exported automatically as
  `CXXFLAGS`.

- **build.dir** (`pkg_build_dir`) — The location of the package build
  directory. Default: `$pkg_work_dir` or `$pkg_work_dir/$pkg_config` if
  `$pkg_config` is set. For packages with the "toolchain" install type it is
  exported automatically.

- **build.generate** (`pkg_build_generate`) — If set to `true`, indicates that
  `autoreconf` stage is necessary for the package. Specifically `autoreconf`
  should be done by running `autogen.sh` script in the source directory. Also
  causes `libtool` to be install for the host.

- **build.generator** (`pkg_build_generator`) — If the package build type is
  'CMake' sets its CMake generator.

- **build.in_source** (`pkg_build_in_source`) — If set to `true`, indicates
  that the package can not be built outside of it's source directory.

- **build.jobs** (`pkg_build_jobs`) — Specifies the number of jobs passed as a
  `-j` argument to the `make` and `ninja` commands. If unset, defaults to the
  number of processors in the system (`nproc`). The most useful value is
  probably "1" which forces a single `make` instance for packages with badly
  written Makefiles.

- **build.kernel_modules** — If set to `true`, indicates that the package
  installs Linux kernel modules.

- **build.ldflags** — Specifies linker flags. It does not set environment
  variables by itself but intended to be used scripts. For the packages with
  the "toolchain" install type it is exported automatically as `LDFLAGS`.

- **build.options** (`pkg_build_options`) — The list of options for the build system.
  It could be `configure` arguments or `CMake` defines or something else
  depending on the package build system.

- **build.profile** (`pkg_build_profile`) — The "profile" of the build.
  Supported values are: release, debug and release_with_debug. This allows
  overriding of the global setting.

- **build.set_toolchain** (`pkg_build_set_toolchain`) — If set to `true`,
  specifies that the package expects the preferred toolchain to be passed in
  the environment (such as in CC, CXX) variables. This is usually the case for
  packages with manually written Makefiles lacking any fallback.

- **build.system** — Specifies the target system. It does not set compiler
  flags by itself but can be used in pkg scripts to do so. For packages with
  the "toolchain" install type it is exported automatically.

- **build.target_requires_host** — If set to `true`, specifies that in order to
  build this package in `target` config the `host` config needs to be built and
  installed first. This is the case, for example, when the package provides
  generator executables which need to be run on the host system. The rule for
  the host package will be derived automatically.

- **build.toolchain** — Specifies the package toolchain which also will be
  added to the package requires. If not set, the default is `gcc-native` if the
  package config name is `host` and `$jagen_target_toolchain` if the config
  name is `target`. The `false` value indicates that the package does not
  require a toolchain. Should be set to `false` for toolchain packages
  themselves to break dependency cycles.

- **build.type** (`pkg_build_type`) — The type of the package build system.
  Supported values are: GNU, CMake, KBuild, make, linux_kernel, linux_module.

### Install

- **install.args** (`pkg_install_args`) — A list of additional arguments for
  install command invocation.

- **install.config_script** (`pkg_install_config_script`) — The location of the
  installed config script relative the the install prefix.

- **install.ldconfig** (`pkg_install_ldconfig`) — Run `ldconfig` after the
  install.

- **install.libs** (`pkg_install_libs`) — A list of names of the libraries
  installed by this package. Used for post-install processing of config
  scripts, `*.la` files and pkg-config files.

- **install.module_dirs** (`pkg_install_module_dirs`) — a list of directories
  relative the the source directory containing Linux kernel modules which could
  be installed using the standard command (`make M=$dir modules_install`) or
  similar

- **install.prefix** (`pkg_install_prefix`) — Install prefix.

- **install.root** (`pkg_install_root`) — Install root.

- **install.type** (`pkg_install_type`) — The type of the build system for the
  purposes of install.  Usually the same is the `build.type` but can be
  overridden. Supported values: GNU, CMake, make, linux_kernel, linux_module,
  none.

### Global variables

- **jagen_FS** — Field separator (`<tab>`).
- **jagen_IFS** — IFS (`<newline><tab>`).
- **jagen_android_product** — The name of the current Android product.
- **jagen_bin_dir** — The location of the `bin` directory (`$jagen_project_dir/bin`).
- **jagen_build_dir** — The location of the `build` directory (`$jagen_project_dir/build`).
- **jagen_build_profile** — Global package build profile.
- **jagen_build_verbose** — Indicates whether the build system should run verbosely.
- **jagen_ccache** — The name of the ccache executable.
- **jagen_cmake_build_options** — Arguments to pass to CMake in `compile` stage.
- **jagen_cmake_generator** (`Ninja`) — Override CMake generator.
- **jagen_cmake_options** — Arguments to pass to CMake in `configure` stage.
- **jagen_cmake_module_path** — The default `CMAKE_MODULE_PATH`.
- **jagen_cmake_toolchain_file** — The `CMAKE_TOOLCHAIN_FILE` to pass to CMake in `configure` stage. It applies only to `target` config.
- **jagen_debug** — Debug level.
- **jagen_dir** — The location of the Jagen source directory.
- **jagen_dist_dir** — The location of the `dist` directory (`$jagen_project_dir/dist`).
- **jagen_flags** — Space-separated list of flags.
- **jagen_host_cmake_module_path** — The `CMAKE_MODULE_PATH` for "host" config.
  Overrides `$jagen_cmake_module_path` if set, empty value disables.
- **jagen_host_dir** — The location of the `host` directory (`$jagen_project_dir/host`).
- **jagen_include_dir** — The location of the `include` directory (`$jagen_project_dir/include`).
- **jagen_kernel_config** — The name of the Linux kernel config to use.
- **jagen_kernel_dir** — The location of the Linux kernel directory.
- **jagen_kernel_image** — The name of the Linux kernel image target (uImage or such).
- **jagen_kernel_modules_dir** — The location of Linux kernel modules install directory.
- **jagen_kernel_release** — The Linux kernel release.
- **jagen_kernel_version** — The Linux kernel version.
- **jagen_layers** — The IFS-separated list of paths for used layers.
- **jagen_lib_dir** — The location of the `lib` dir (`$jagen_dir/lib`).
- **jagen_log_dir** — The location of the `log` dir (`$jagen_build_dir`).
- **jagen_lua** — The name of the Lua executable.
<!-- - **jagen_out_dir** — _Not used_. -->
- **jagen_pager** — Overrides PAGER environment variable.
- **jagen_path** — Import path. Derived from layer locations.
- **jagen_private_dir** — The location of the private directory (can be set from project-specific config).
- **jagen_product** — _init-project_.
- **jagen_product_dir** — _init-project_.
- **jagen_project_dir** — The location of the current project directory.
- **jagen_project_lib_dir** — The location of the current project `lib` directory (`$jagen_project_dir/lib`).
- **jagen_relative_dir** — _env.sh_ internal.
- **jagen_sdk** — The name of the current SDK.
- **jagen_sdk_dir** — The location of the current SDK.
<!-- - **jagen_sdk_ezboot_dir** — _Sigma layer specific._ -->
<!-- - **jagen_sdk_mrua_dir** — _Sigma layer specific._ -->
<!-- - **jagen_sdk_rootfs_dir** — _Sigma layer specific._ -->
<!-- - **jagen_sdk_rootfs_prefix** — _Sigma layer specific._ -->
<!-- - **jagen_sdk_rootfs_root** — _Sigma layer specific._ -->
<!-- - **jagen_sdk_staging_dir** — _AST vendor specific._ -->
<!-- - **jagen_sdk_tools_dir** — _HiSilicon layer specific._ -->
- **jagen_shell** — Override shell used to run internal scripts.
<!-- - **jagen_sigma_cpukeys** — _Sigma layer specific._ -->
<!-- - **jagen_sigma_xsdk_dir** — _Sigma layer specific._ -->
- **jagen_source_exclude** — The space-separated list of package names which should be excluded from fetching.
- **jagen_src_dir** — The location of the `src` directory (`$jagen_project_dir/src`).
- **jagen_target_board** — The name of the current target board.
- **jagen_target_cmake_module_path** — The `CMAKE_MODULE_PATH` for "target"
  config. Overrides `$jagen_cmake_module_path` if set, empty value disables.
- **jagen_target_dir** — The location of the `target` directory (`$jagen_project_dir/target`).
- **jagen_target_platform** — The name of the current target platform (currently used only for Android).
- **jagen_target_toolchain** — The name of the current target toolchain.
- **jagen_toolchains_dir** — The location of shared toolchains (`$jagen_project_dir/toolchains`).

#### Stage-specific variables

- **pkg_args** — stage arguments

- **pkg_build_cmake_module_path** (`build.cmake_module_path`) — the
  `CMAKE_MODULE_PATH` for the package; default: `$jagen_cmake_module_path`

- **pkg_build_cmake_toolchain_file** (`build.cmake_toolchain_file`) —
  `CMAKE_TOOLCHAIN_FILE` for the current package

- **pkg_build_configure_file** (`build.configure_file`) — the path to the
  configure file; default: `$pkg_source_dir/configure`

- **pkg_build_configure_needs_install_dir**
  (`build.configure_needs_install_dir`) — if set, specifies that the
  `configure` requires libraries and include directories from the package
  install directory to find dependencies; effectively this setting adds
  `-I$pkg_install_dir/include` to `CFLAGS` and `-L$pkg_install_dir/lib` to
  `LDFLAGS`.

- **pkg_build_dir** (`build.dir`) — the location of the package build directory

- **pkg_build_generate** (`build.generate`) — if build type is "GNU" and
  `autogen.sh` is found in the source directory — run it

- **pkg_build_generator** (`build.generator`) — if build type is "CMake" sets
  per-package CMake generator option (passed in -G argument).

- **pkg_build_in_source** (`build.in_source`) — if set, specifies that the
  location of the "build" directory is the same as the "source" directory; also
  indicates that the package build system do not support "out-of-source" builds
  or it does not work for some reason; can change other behaviour apart from
  setting build dir = source dir

- **pkg_build_options** (`build.options`) — passed as and argument to the
  underlying build system directly, it could be `configure` options, `CMake`
  defines or `make` variable assignments depending on the package build type

- **pkg_build_profile** (`build.profile`) — specifies a "build profile" for the
  package (usually this setting is called the "configuration" in other build
  systems, but in Jagen "config" has another meaning); supported values:
  release, debug, release_with_debug

- **pkg_build_toolchain** (`build.toolchain`) — the name of the toolchain
  package which should be used for the build

- **pkg_build_type** (`build.type`) — the type of the build system of the
  package; currently supported values are: GNU, KBuild, CMake, make,
  linux_kernel, linux_module

- **pkg_config** (`pkg.config`) — the config of the currently executing stage

- **pkg_install_args** (`install.args`) — a list of additional install command
  arguments

- **pkg_install_config_script** (`install.config_script`) — specifies the
  location of the `*-config` script relative to the sysroot; some GNU packages
  install those with hardcoded values which might need post-install cleanup

- **pkg_install_dir** — full path to the package installation directory
  (`$pkg_install_root$pkg_install_prefix`)

- **pkg_install_ldconfig** (`install.ldconfig`) — a flag indicating whether to
  run `ldconfig` after the install

- **pkg_install_libs** (`install.libs`) — a list of names of the libraries
  installed by this package

- **pkg_install_module_dirs** (`install.module_dirs`) — a list of directories
  relative the the source directory containing Linux kernel modules which could
  be installed using the standard command (`make M=$dir modules_install`) or
  similar

- **pkg_install_prefix** (`install.prefix`) — specifies the install prefix of
  the package, depends on the build system (`--prefix=` passed to `configure`
  script or `CMAKE_INSTALL_PREFIX` for `CMake`)

- **pkg_install_root** (`install.root`) — specifies the "root" of the package
  installation; can have different interpretation depending on the build system
  but in general used for various autotools workaround (cleaning `.la` and
  `.pc` files and such); sets `DESTDIR` for `make` and `CMake`

- **pkg_install_type** (`install.type`) — sets the type of the build system for
  the "install" stage; generally the same as the "build type" but can differ if
  overridden; supported values: GNU, make, CMake, linux_kernel, linux_module,
  none

- **pkg_name** — the package name of the currently executing stage

- **pkg_patches_provided** (`pkg.patches.provided`) — a list of absolute patch
  filenames which the current package is expected to provide

- **pkg_patches_required** (`pkg.patches.required`) — a list of absolute patch
  filenames which the current package requires

- **pkg_query** — query

- **pkg_run_on_error** — error hook

- **pkg_source** (`source.type source.location`) — concatenated type and
  location into a single variable for the historical reasons (in the past there
  was a shorthand allowed to not set type which is then assumed as a `dist`)

- **pkg_source_basename** (`source.basename`) — the "basename" of source (the
  filename without an extension, `.git` is also considered an extension)

- **pkg_source_branch** (`source.branch`) — the source branch to checkout for
  source-based packages

- **pkg_source_dir** (`source.dir`) — the source directory of the package

- **pkg_source_exclude** (`source.exclude`) — if set to any value than the
  "unpack" stage will not clean or update the package source directory (this
  rule takes precedence over the "ignore\_dirty")

- **pkg_source_filename** (`source.filename`) — the "filename" part of the
  location (the part after the last "/" if not set manually)

- **pkg_source_ignore_dirty** (`source.ignore_dirty`) — if set to any value
  then the "unpack" stage will cleanup the source directory even if it has
  changes

- **pkg_source_md5sum** (`source.md5sum`) — MD5 hash of the source file (for
  dist sources only)

- **pkg_source_sha1sum** (`source.sha1sum`) — SHA1 hash of the source file (for
  dist sources only)

- **pkg_source_sha256sum** (`source.sha256sum`) — SHA256 hash of the source
  file (for dist sources only)

- **pkg_stage** — the name of the currently executing stage (unpack, patch,
  configure, install, etc.)

- **pkg_work_dir** (`pkg.work_dir`) — a location of the toplevel working
  directory for the package; can contain unpacked sources, several build
  directories, etc. depending on the configuration

## Build system internals

The build system is generated from a set of rules represented by tables
(key-value pairs) which are found in `rules.lua` and `pkg/<name>.lua` files
across layer directories. Each rule defines some piece of information about a
package: build stages, type, location of source directory and so on. Rules with
the same name are considered as belonging to the same package but evaluated
independently at the point of reference. So, mentioning the package in the
"requires" list of a package which has template and config will produce
different result from standalone "package" declaration. Also order of rules
matters, both in the rules file and across layers. See the `package`
function in `src/Package.lua` file for complete information about evaluation
logic.

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
