# User Manual

## Contents

- [Initializing](Initializing.md)
- [Building](Building.md)
- [Cleaning](Cleaning.md)
- [Targets](#targets)
- [Working with source packages](ManagingSources.md)
- [Manage filesystem images](Images.md)
- [Install Bash completions](Installation.md)
- [Writing build scripts](#writing-build-scripts)
- [Build system internals](#build-system-internals)

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

Rules are processed by the `define_rule` function and should be Lua table of
the following form:

```lua
define_rule { 'name', '[config]',
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

        libs    = { 'name1', 'name2', ... },
        options = { 'option1', 'option2', ... },
        kernel_modules = true,

        in_source = true,
        dir       = 'path',
        work_dir  = 'path',
        requires_toolchain = false,
        profile   = 'release|debug|release_with_debug',
    },
    install = {
        type    = 'GNU|CMake|make|linux_kernel|linux_module|none',

        prefix  = 'path',
        root    = 'path',

        config_script = 'filename',
        modules = { 'name1', 'name2', ...},
    },
    requires = {
        'name1',
        'name2',
        ...
    },
}
```

- **pkg.name** (`pkg_name`) — the name of the package, it is derived from the
  rule and do not need to be set explicitly

- **pkg.config** — a config for the current pkg rule, also derived from the
  rule or can be set by a template

- **pkg.filenames** — a list of filenames where rules for this package was
  found

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

- **pkg.pass_template** — A template which is passed to the derived rules but
  not applied to the current rule itself. It is useful to break cycles of
  requires.

- **pkg.stages** — Stores rule targets. _Internal._

- **pkg.configs** — Stores config-specific rules and targets. _Internal._

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

- **build.autoreconf** — If set to `true`, indicates that `autoreconf` stage is
  necessary for the package. Also causes `libtool` to be installed for the
  host.

- **build.configure_file** (`pkg_configure_file`) — The location of the
  `configure` script (`$pkg_source_dir/configure`).

- **build.configure_needs_install_dir** (`pkg_configure_needs_install_dir`) —
  if set, specifies that the `configure` requires libraries and include
  directories from the package install directory to find dependencies;
  effectively this setting adds `-I$pkg_install_dir/include` to `CFLAGS` and
  `-L$pkg_install_dir/lib` to `LDFLAGS`.

- **build.dir** (`pkg_build_dir`) — The location of the package build
  directory. Default: `$pkg_work_dir` or `$pkg_work_dir/$pkg_config` if
  `$pkg_config` is set.

- **build.generate** (`pkg_build_generate`) — If set to `true`, indicates that
  `autoreconf` stage is necessary for the package. Specifically `autoreconf`
  should be done by running `autogen.sh` script in the source directory. Also
  causes `libtool` to be install for the host.

- **build.generator** (`pkg_build_generator`) — If the package build type is
  'CMake' sets its CMake generator.

- **build.in_source** (`pkg_build_in_source`) — If set to `true`, indicates
  that the package can not be built outside of it's source directory.

- **build.kernel_modules** — If set to `true`, indicates that the package
  installs Linux kernel modules.

- **build.libs** (`pkg_libs`) — The list of libraries installed by this
  package.

- **build.options** (`pkg_options`) — The list of options for the build system.
  It could be `configure` arguments or `CMake` defines or something else
  depending on the package build system.

- **build.profile** (`pkg_build_profile`) — The "profile" of the build.
  Supported values are: release, debug and release_with_debug. This allows
  overriding of the global setting.

- **build.requires_toolchain** — If set to `false`, indicates that the package
  does not need toolchain. Used internally for the "toolchain-like" packages
  themselves to break dependency cycles.

- **build.type** (`pkg_build_type`) — The type of the package build system.
  Supported values are: GNU, CMake, KBuild, make, linux_kernel, linux_module,
  maven.

- **build.work_dir** (`pkg_work_dir`) — The working directly for the package.
  Default value: `$jagen_build_dir/$pkg_name`.

### Install

- **install.config_script** (`pkg_install_config_script`) — The location of the
  installed config script relative the the install prefix.

- **install.modules** (`pkg_install_modules_dirs`) — Linux kernel modules.

- **install.prefix** (`pkg_prefix`) — Install prefix.

- **install.root** (`pkg_sysroot`) — Install root.

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
- **jagen_cmake_module_path** — Override the `CMAKE_MODULE_PATH`.
- **jagen_cmake_options** — Arguments to pass to CMake in `configure` stage.
- **jagen_debug** — Debug level.
- **jagen_dir** — The location of the Jagen source directory.
- **jagen_dist_dir** — The location of the `dist` directory (`$jagen_project_dir/dist`).
- **jagen_flags** — Space-separated list of flags.
- **jagen_host_cflags** — The global host cflags.
- **jagen_host_dir** — The location of the `host` directory (`$jagen_project_dir/host`).
- **jagen_host_system** — The name of the current host system.
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
- **jagen_mvn_exe** — A name or a path to the mvn executable.
- **jagen_mvn_options** — A list of arguments which are passed to every invocaction of mvn.
<!-- - **jagen_out_dir** — _Not used_. -->
- **jagen_pager** — Overrides PAGER environment variable.
- **jagen_path** — Import path. Derived from layer locations.
- **jagen_private_dir** — The location of the private directory (can be set from project-specific config).
- **jagen_product** — _init-project_.
- **jagen_product_dir** — _init-project_.
- **jagen_project_dir** — The location of the current project directory.
- **jagen_project_lib_dir** — The location of the current project `lib` directory (`$jagen_project_dir/lib`).
- **jagen_relative_dir** — _env.sh_ internal.
- **jagen_root_dir** — _config.sh_ internal.
- **jagen_sdk** — The name of the current SDK.
- **jagen_sdk_dir** — The location of the current SDK.
<!-- - **jagen_sdk_ezboot_dir** — _Sigma layer specific._ -->
<!-- - **jagen_sdk_initfs_dir** — _Sigma layer specific._ -->
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
- **jagen_target_arch** — The name of the current target architecture.
- **jagen_target_board** — The name of the current target board.
- **jagen_target_cflags** — The global target cflags.
- **jagen_target_cpu** — The name of the current target CPU (like cortex-a9).
- **jagen_target_dir** — The location of the `target` directory (`$jagen_project_dir/target`).
- **jagen_target_platform** — The name of the current target platform (currently used only for Android).
- **jagen_target_system** — The name of the current target system.
- **jagen_target_toolchain** — The name of the current target toolchain.
- **jagen_target_toolchain_dir** — The location of the current target toolchain directory.
<!-- - **jagen_toolchain_dir** — Toolchain dir. __TODO CHECK USAGES__ -->
- **jagen_toolchain_prefix** — The current toolchain prefix.
- **jagen_toolchains_dir** — The location of the `toolchains` directory (`$jagen_project_dir/toolchains`).
- **jagen_vendor** — The name of the current vendor.

#### Stage-specific variables

- **pkg_args** — stage arguments

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

- **pkg_build_profile** (`build.profile`) — specifies a "build profile" for the
  package (usually this setting is called the "configuration" in other build
  systems, but in Jagen "config" has another meaning); supported values:
  release, debug, release_with_debug

- **pkg_build_type** (`build.type`) — the type of the build system of the
  package; currently supported values are: GNU, KBuild, CMake, make,
  linux_kernel, linux_module

- **pkg_config** (`pkg.config`) — the config of the currently executing stage

- **pkg_configure_file** (`build.configure_file`) — specifies the path to the
  configure file; `$pkg_source_dir/configure` if unset

- **pkg_configure_needs_install_dir** (`build.configure_needs_install_dir`) —
  if set, specifies that the `configure` requires libraries and include
  directories from the package install directory to find dependencies;
  effectively this setting adds `-I$pkg_install_dir/include` to `CFLAGS` and
  `-L$pkg_install_dir/lib` to `LDFLAGS`.

- **pkg_install_config_script** (`install.config_script`) — specifies the
  location of the `*-config` script relative to the sysroot; some GNU packages
  install those with hardcoded values which might need post-install cleanup

- **pkg_install_dir** — full path to the package installation directory
  (`$pkg_sysroot$pkg_prefix`)

- **pkg_install_modules_dirs** (`install.modules`) — a list of directories
  relative the the source directory containing Linux kernel modules which could
  be installed using the standard command (`make M=$dir modules_install`) or
  similar

- **pkg_install_type** (`install.type`) — sets the type of the build system
  for the "install" stage; generally the same as the "build type" but can
  differ if overridden; supported values: GNU, make, CMake, linux_kernel,
  linux_module, none

- **pkg_libs** (`build.libs`) — a list of names of the libraries installed by
  this package; used for post-install processing of config scripts, `la` files
  and pkg-config files

- **pkg_log** — log path

- **pkg_name** — the package name of the currently executing stage

- **pkg_options** (`build.options`) — passed as and argument to the underlying
  build system directly, it could be `configure` options, `CMake` defines or
  `make` variable assignments depending on the package build type

- **pkg_patches_provided** (`pkg.patches.provided`) — a list of absolute patch
  filenames which the current package is expected to provide

- **pkg_patches_required** (`pkg.patches.required`) — a list of absolute patch
  filenames which the current package requires

- **pkg_prefix** (`install.prefix`) — specifies the install prefix of the
  package, depends on the build system (`--prefix=` passed to `configure`
  script or `CMAKE_INSTALL_PREFIX` for `CMake`)

- **pkg_query** — query

- **pkg_run_jobs** — jobs

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

- **pkg_source_ignore_dirty** (`source.ignore_dirty`) — if set to any value
  then the "unpack" stage will cleanup the source directory even if it has
  changes

- **pkg_source_filename** (`source.filename`) — the "filename" part of the
  location (the part after the last "/" if not set manually)

- **pkg_source_md5sum** (`source.md5sum`) — MD5 hash of the source file (for
  dist sources only)

- **pkg_source_sha1sum** (`source.sha1sum`) — SHA1 hash of the source file (for
  dist sources only)

- **pkg_source_sha256sum** (`source.sha256sum`) — SHA256 hash of the source
  file (for dist sources only)

- **pkg_stage** — the name of the currently executing stage (unpack, patch,
  configure, install, etc.)

- **pkg_sysroot** (`install.root`) — specifies the "root" of the package
  installation; can have different interpretation depending on the build system
  but in general used for various autotools workaround (cleaning `.la` and
  `.pc` files and such); sets `DESTDIR` for `make` and `CMake`

- **pkg_system** — the value of `$jagen_*_system` for the currently executing
  stage depending on the current config

- **pkg_work_dir** (`build.work_dir`) — a location of the toplevel working
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
different result from standalone "define_rule" declaration. Also order of rules
matters, both in the rules file and across layers. See the `define_rule`
function in `src/Package.lua` file for complete information about evaluation
logic.

Package dependencies and build commands are written to `build.ninja` file in
the `build` directory, additional package specific environment goes into
"include scripts" in the `include` directory. All code dealing with include
script generation is in `src/Script.lua` file which can be used as a reference.
Dependencies are resolved by touching specifically named files in the build
directory after the command succeeded, see the generated `build.ninja` for
details.

At the core of the build system is a `jagen-pkg` script. Given a package name,
stage and config it finds and imports all necessary environment files, runs the
stage script and returns its result. Default build stage scripts and utility
functions are placed in a `src/pkg.sh` file which can be used as a reference.
Every `pkg/<name>.lua` file can have `pkg/<name>.sh` backing file in the same
directory which is included by `jagen-pkg` during the build and can be used to
override default stages or environment.

Layers, build type and directory locations are set in 'config.sh' which is
generated during project initialization. It is also included indirectly in
every build command.
