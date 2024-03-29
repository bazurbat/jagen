# Rules

The build system is generated from a set of rules loaded from `rules.lua` files found across Jagen
path.

The Jagen path is formed from layer directories in the reverse order with the `lib` directory of
the current project prepended and the `lib` directory in the associated Jagen source repository
appended. The current value of the Jagen path is stored in the `$jagen_path` variable.

The rule files are executed starting from the last one found in Jagen path so that definitions from
the project are appended to the definitions from layers which are appended to generic definitions
or to the definitions from earlier layers.

Packages in the rule files are defined using the `package` function with a single "table" argument
such as:

```lua
package { 'nanomsg',
    source = 'https://github.com/nanomsg/nanomsg.git',
    build  = 'cmake'
}
```

This rule defines a package named "nanomsg" which should be downloaded from
https://github.com/nanomsg/nanomsg.git and built using CMake. It is a shorthand for the full form:

```lua
package { 'nanomsg',
    source = {
        type = 'git',
        location = 'https://github.com/nanomsg/nanomsg.git'
    },
    build = {
        type = 'cmake'
    }
}
```

Package rules are merged recursively in order with the value type properties (string, number and
boolean) override previous values and array type properties are appended. This means that later
rule of the form:

```lua
package { 'nanomsg',
    build = {
        profile = 'debug',
        options = {
            'a', 'b'
        }
    }
}
```
will effectively result in the following rule:

```lua
package { 'nanomsg',
    source = {
        type = 'git',
        location = 'https://github.com/nanomsg/nanomsg.git'
    },
    build = {
        type = 'cmake',
        profile = 'debug',
        options = {
            'a', 'b'
        }
    }
}
```
and an additional rule:

```lua
package { 'nanomsg',
    source = {
        location = 'https://github.com/me/myfork.git'
    },
    build = {
        profile = 'release',
        options = {
            'c'
        }
    }
}
```
will result in:

```lua
package { 'nanomsg',
    source = {
        type = 'git',
        location = 'https://github.com/me/myfork.git'
    },
    build = {
        type = 'cmake',
        profile = 'release',
        options = {
            'a', 'b', 'c'
        }
    }
}
```

## Package files

Packages can be defined in "package files" separately from rules. These files serve as a "baseline"
definition of a package and are applied the first time the given package name is encountered as if
the definition was given directly before the rule. Contrary to the definitions in rules the
definitions from package files are not merged across layers, the first one which is found will be
used, the later ones are not considered. This is the main mechanism of overriding in Jagen. Also,
generally, the package files should not have a config, and define things like source, build type
and custom stages which should be shared between all configs of the package.

The package files are executed as Lua files and are expected to return the package definition, the
typical is the form:

```lua
return {
    source = {
        type = 'git'
        location = 'https://github.com/nanomsg/nanomsg.git'
    },
    build = {
        type = 'cmake'
    }
}
```

Note the `return` instead of `package` and no name or config. The name is derived implicitly from
the package file name.

These package files are searched in `$jagen_path` in a manner similar to rules but in a `pkg`
subdirectory of each layer. For a given package `zeromq~var1` the search path will be:

    pkg/zeromq~var1/zeromq.lua
    pkg/zeromq~var1.lua
    pkg/zeromq/zeromq.lua
    pkg/zeromq.lua

## Variants

Package rules can inherit properties from already defined packages using `extends` declaration.
Those "extended" packages are called "variants" and can add or override properties from their base.
This facility provides a shorthand to declare a group of packages with minor differences.

For example, the definitions:

```lua
package { 'nanomsg', 'host',
    source = {
        type = 'git'
        location = 'https://github.com/nanomsg/nanomsg.git'
    },
    build = {
        type = 'cmake',
        options = { 'param1', 'param2' }
    }
}
package { 'nanomsg~stable', 'host',
    extends = 'nanomsg',
    source = { branch = 'stable' },
    build = { options = 'override' }
}
package { 'nanomsg~dev', 'host',
    extends = 'nanomsg',
    source = { branch = 'dev' },
    build = { options = { 'add' } }
}
```

effectively have the same effect as the:

```lua
package { 'nanomsg', 'host',
    source = {
        type = 'git'
        location = 'https://github.com/nanomsg/nanomsg.git'
    },
    build = {
        type = 'cmake',
        options = { 'param1', 'param2' }
    }
}
package { 'nanomsg~stable', 'host',
    source = {
        type = 'git'
        location = 'https://github.com/nanomsg/nanomsg.git',
        branch = 'stable'
    },
    build = {
        type = 'cmake',
        options = { 'override' }
    }
}
package { 'nanomsg~dev', 'host',
    source = {
        type = 'git'
        location = 'https://github.com/nanomsg/nanomsg.git',
        branch = 'dev'
    },
    build = {
        type = 'cmake',
        options = { 'param1', 'param2', 'add' }
    }
}
```

Note the way build options are merged. Basically, building different branches with different
options at the same time is the main purpose this feature was designed for. Useful for testing and
CI.

The variants are considered separate packages and should have different names. As a consequence all
of them will be cloned to different directories (`$jagen_src_dir/$pkg_name` by default) and built
separately.

The final definition (after merging rules from all layers) for the base package is copied and
properties from the extending rule are applied on top. Variant rules are not merged with each other
as normal rules do, only the latest rule takes an effect replacing the previous definitions if any.

You can "use spec" syntax to explicitly request a config from a base package:

```lua
package { 'nanomsg2',
    extends = 'nanomsg:host'
}
```

which has the same effect as:

```lua
package { 'nanomsg2', 'host',
    extends = 'nanomsg'
}
```

If the config is not specified then all configs from the base package will be copied.

## Properties

All possible package properties are described below. The first name is the key to use in the
package definition, the second name (in the parentheses) is the name of the shell variable which
can be used in custom scripts or as the part of another property value.

```lua
{
    source = {
        type     = 'dist|git|hg|repo',
        location = 'filename|URL',

        md5sum    = 'hash string',
        sha1sum   = 'hash string',
        sha256sum = 'hash string',

        dir      = 'path',
        basename = 'filename',
        filename = 'filename',
        exclude  = true,

        ignore_dirty = false
    },
    patches = {
        'filename1',
        { 'filename2', num },
        ...
    },
    files = {
        'filename1',
        { 'filename2' },
        { 'filename3, dir='/some/path' },
        { 'filename4', path='/some/path/dest_filename' },
    },
    build = {
        type = 'gnu|cmake|kbuild|make|linux-kernel|linux-module',

        generate   = true, -- or 'autogen' or 'autoconf'
        generator  = 'Ninja',
        configure_file = 'path',

        options = { 'option1', 'option2', ... },
        kernel_modules = true,

        in_source = true,
        dir       = 'path',
        profile   = 'release|debug|release_with_debug',
    },
    install = {
        type    = 'gnu|cmake|make|linux-kernel|linux-module',

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

- **name** (`pkg_name`) — The name of the package. It is derived from the rule and do not need to
  be set explicitly.

- **config** (`pkg_config`) — The config for the current pkg rule, also derived from the rule.

- **source** (`pkg_source_*`) — A Source object.

- **patches** — A list of patch files to automatically apply during the "patch" stage.

- **build** (`pkg_build_*`) — Parameters for the build stage.

- **install** (`pkg_install_*`) — Parameters for the install stage.

- **requires** — A list of dependencies.

- **template** — A name or a list of names of templates for the current rule. The templates are
  merged in order then the current rule is merged with the result. This way value type properties
  (such as strings and numbers) from the rule override values from the templates and array type
  values are appended. The mentioned templates should be defined with the `template` function
  beforehand.

- **work_dir** (`pkg_work_dir`) — The working directly for the package. Default value:
  `$jagen_build_dir/$pkg_name`.

## Source

- **source.basename** (`pkg_source_basename`) — The part of the source filename without the last
  extension (`.git` and `.hg` are also considered extensions). It is used as the last component of
  `source.dir` for dist sources and for SCM sources if the `source.name` is unset. Adjust this
  property in the case when the base directory stored inside the `tar` archive differs from its
  filename.

- **source.bookmark** — The bookmark to pull on update (Hg sources only). Can be a list in which
  case the last item is used a bookmark which is used for the work directory.

- **source.branch** — The branch to checkout/fetch on update (SCM sources only). Can be a list in
  which case the last item will be used as a branch to checkout.

- **source.dir** (`pkg_source_dir`) — A path to the package source directory. Defaults to
  `$jagen_src_dir/source.name` for SCM sources and `$jagen_build_dir/source.name/source.basename`
  for dist sources. If the path does not start with `/`, `$`, space or control character it is
  interpreted as a relative to the default directory, otherwise it is taken verbatim and considered
  an absolute path.

- **source.exclude** (`pkg_source_exclude`) — If set to `true` indicates that the source should not
  be updated during the "unpack" stage and should not be patched during the "patch" stage. This
  generally should be set for all manually managed and "work in progress" packages because having
  unsaved changes in the source directory during the update is considered an error otherwise.

- **source.exclude_submodules** — If set to `true` indicates that Git submodules found in the
  repository should be ignored, i.e. not initialized or updated automatically when switching
  branches.

- **source.filename** (`pkg_source_filename`) — The part of the source location after the last `/`
  or the whole location if it does not contain `/`. Adjust this property to store the downloaded
  dist source under a different name than the last component of the URL.

- **source.force_update** (`pkg_source_force_update`) — If set to `true` indicates that the source
  is force pushed on the upstream, so Jagen will update by `reset` instead of `merge`. Relevant
  only for Git sources. If this is not set the failure to merge will be considered an error.

- **source.ignore\_dirty** — Ignore "dirty" status of the source directory and try to update/patch
  it anyway.

- **source.location** — The location of the source file or the repository URL.

- **source.md5sum** (`pkg_source_md5sum`) — MD5 hash of the source file (for dist sources only)

- **source.name** (`pkg_source_name`) — User defined name of the source. Defaults to the package
  name which contains the current source definition. Also used as the last component of
  `source.dir` for SCM sources, i.e. it sets the name of the directory where the source is checked
  out under the `$jagen_src_dir`.

- **source.sha1sum** (`pkg_source_sha1sum`) — SHA1 hash of the source file (for dist sources only)

- **source.sha256sum** (`pkg_source_sha256sum`) — SHA256 hash of the source file (for dist sources
  only)

- **source.shallow** (`pkg_source_shallow`) — Specifies that the source should be cloned with the
  history truncated to the latest commit only (depth 1). Applies only for Git. Default: `true`, set
  to `false` explicitly to override. If this setting is changed to `false` when the source is
  already cloned then Jagen will unshallow it during the next fetch.

- **source.rev** — A revision identifier to checkout or update. Takes priority over other revision
  specifiers (branch, tag, bookmark), the interpretation depends on the underlying SCM.

- **source.tag** — The tag (Git) or revision (Hg) to checkout and fetch on update, takes priority
  over a branch. Can be a list in which case the last item is the tag to checkout.

- **source.type** — A type of the source: dist, git, hg, repo. The packages with the source types
  of "git", "hg" and "repo" are considered "SCM" packages and can be managed by the `jagen src`
  command.

## Patches

Packages can specify a list of patch files to apply during the "patch" stage in the `patches`
property.

Each item can be either a filename string or an object of the form:
```lua
{ 'filename', num }
```
where `filename` is the file name of the patch and the `num` is the number of leading slashes to
strip from paths when applying the patch (passed as `-pnum` to the `patch` utility). The `num`
is optional and defaults to `1` if not specified.

For each patch filename `$name` Jagen tries to find the corresponding `pkg/$pkg_name/$name` file
across the `$jagen_path`. If the package name has the variant suffix (`~suffix`) it also tries the
paths with the suffix removed. Found patch files are added as dependencies of the "unpack" stage of
the package, so the changes to the patch files will cause the rebuild. For example, given the
definition:
```lua
package { 'mypackage~ver1',
    patches = {
        'filename1.patch',
    }
}
```
layers:
```sh
jagen_layers='
../layer1
../layer2
'
```
and Jagen project in `~/work/project` and assuming the Jagen repo is in the default location the
search order will be the following:
```
~/work/project/lib/pkg/mypackage~ver1/filename1.patch
~/work/project/lib/pkg/mypackage/filename1.patch
~/work/layer2/pkg/mypackage~ver1/filename1.patch
~/work/layer2/pkg/mypackage/filename1.patch
~/work/layer1/pkg/mypackage~ver1/filename1.patch
~/work/layer1/pkg/mypackage/filename1.patch
~/work/project/.jagen/lib/pkg/mypackage~ver1/filename.patch
~/work/project/.jagen/lib/pkg/mypackage/filename.patch
```
The first filename found will be used. Note that common variant path (without the `~suffix`) from
later layers overrides the more specific variants from earlier layers.

The `patches` property can be set to `false` to remove the list of patches set by upper layers. Use
it to start a new list or in the case when the source type is changed to SCM but the upper layer
declares `dist` source with patches. For example, given the definition:
```lua
-- in layer1
package { 'mypackage',
    source = 'https://somewhere/mypackage.tar.gz',
    patches = {
        'somepatch1'
    }
}
```
and the rule:
```lua
-- in layer2
package { 'mypackage',
    patches = {
        'somepatch2'
    }
}
```
the resulting rule will be:
```lua
package { 'mypackage',
    patches = {
        'somepatch1',
        'somepatch2'
    }
}
```
but you can reset the list with the following pattern:
```lua
-- in layer2
package { 'mypackage',
    patches = false
}
package { 'mypackage',
    patches = {
        'somepatch2'
    }
}
```
`patches = false` will clean the previous value and the next rule just adds to the empty list.

For rules declaring sources with SCM type such as git and hg the patches property is set to `false`
implicitly which means that version controlled repositories will not be patched. If you want to
apply previously declared patches anyway set the `patches` property to empty list `patches = {}` to
suppress the implicit patch list reset.

## Files

The "files" property specifies a list of supplementary files for the package. These files are added
as inputs to the "patch" stage and copied into the specified path when this stage is executed.

Each item in the list can be either a string or an object of the form:
```lua
{ 'filename', dir='/target/directory', path=/target/path/with/filename' }
```
`dir` defines the target directory where the `filename` should be copied and `path` defines full
destination path. Use `path` if you want to have different source and target file names. The `path`
property takes precedence over `dir` if both are specified. Having only `dir` effectively means
`path=dir/filename`. If neither is set the file will be copied to the package build directory
(`$pkg_build_dir`).

The `filename` is searched in the package-specific directory across layers using the same algorithm
as for patch files. It can be a relative path such as `files/filename` so you can organize large
collections of supplementary files.

There are syntax shortcuts available for the property, the following rules have the same meaning:
```lua
files = 'filename1'
files = { 'filename1' }
files = { { 'filename1' } }
```

## Build

- **build.arch** — Specifies the target machine architecture. Derived from
  `build.system` if unset, inherited from the toolchain otherwise. Set to
  `false` to use inherited value even if the `build.system` is set. Exported
  automatically.

- **build.cflags** — Specifies additional C compiler flags. Exported
  automatically.

- **build.clean** — The directory or a list of directories to clean by the
  "clean" command or "build" command with "--clean" option when this package is
  specified as the target. Defaults to **build.dir** when not specified.

- **build.cmake_executable** (`pkg_build_cmake_executable`) — The path to cmake
  executable. Defaults to `$toolchain_cmake_executable` or
  `$jagen_<config>_cmake_executable` or `$jagen_cmake_executable` whichever is
  set, or to `cmake` as the final default.

- **build.cmake_generator** (`pkg_build_cmake_generator`) — Sets the CMake
  generator for the package. Defaults to `$toolchain_cmake_generator` or
  `$jagen_<config>_cmake_generator` or `$jagen_cmake_generator` whichever is
  set, or to `Ninja` as the final default.

- **build.cmake_module_path** (`pkg_build_cmake_module_path`) — Defines
  `CMAKE_MODULE_PATH` for the package. Defaults to
  `$toolchain_cmake_module_path` or `$jagen_<config>_cmake_module_path` or
  `$jagen_cmake_module_path` whichever is set.

- **build.cmake_options** (`pkg_build_cmake_options`) — Additional
  CMake-specific configure options. Defaults to `$toolchain_cmake_options` or
  `$jagen_<config>_cmake_options` or `$jagen_cmake_option` whichever is set.

- **build.cmake_toolchain_file** (`pkg_build_cmake_toolchain_file`) — specifies
  `CMAKE_TOOLCHAIN_FILE` for the package. Defaults to
  `$toolchain_cmake_toolchain_file` or `$jagen_<config>_cmake_toolchain_file`
  or `$jagen_cmake_toolchain_file` whichever is set. If not set for 'target'
  config Jagen will generate a basic toolchain file for cross-compilation in
  the package's build dir.

- **build.configure_file** (`pkg_build_configure_file`) — The location of the
  `configure` script (`$pkg_source_dir/configure`).

- **build.configure_needs_install_dir** (`pkg_build_configure_needs_install_dir`) —
  _Deprecated_. An alias for **build.with_install_dir**.

- **build.cpu** — Specifies the target machine CPU. Assigned from toolchain if
  unset. Exported automatically.

- **build.cxxflags** — Specifies additional C++ compiler flags. Copied from the
  **build.cflags** if unset. Exported automatically.

- **build.dir** (`pkg_build_dir`) — The location of the package build
  directory. Default: `$pkg_work_dir` or `$pkg_work_dir/$pkg_config` if
  `$pkg_config` is set. Exported automatically.

- **build.generate** (`pkg_build_generate`) — Indicates that the package's build system needs to be
  regenerated before running configure. Relevant only for the "gnu" build type. If set to `true` or
  a string "autogen" the `./autogen.sh` script will be ran at the end of the 'patch' stage if found
  in the source directory and `autoreconf` command otherwise. If set to a string "autoreconf" the
  `autoreconf` command will be tried skipping `autogen.sh` script even if it exists.

- **build.generator** (`pkg_build_generator`) — _Deprecated._ An alias for
  **build.cmake_generator** for backward compatibility.

- **build.in_source** (`pkg_build_in_source`) — If set to `true`, indicates that the package can not be
  built outside of its source directory. Can be set to `multi` to indicate that the package supports
  multi-configuration builds inside of its source directory.

- **build.jobs** (`pkg_build_jobs`) — Specifies the number of jobs passed as a
  `-j` argument to the `make` and `ninja` commands. Defaults to `jagen_jobs` or
  the number of processors in the system (`nproc`) if unset.  The most useful
  value is probably "1" which forces a single `make` instance for packages with
  badly written Makefiles.

- **build.kernel_modules** — If set to `true`, indicates that the package
  installs Linux kernel modules.

- **build.ldflags** — Specifies additional linker flags. Exported
  automatically.

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

- **build.system** — Specifies the target system. Assigned from toolchain if
  unset. Exported automatically.

- **build.target_requires_host** — If set to `true`, specifies that in order to
  build this package in `target` config the `host` config needs to be built and
  installed first. This is the case, for example, when the package provides
  generator executables which need to be run on the host system. The rule for
  the host package will be derived automatically.

- **build.toolchain** — Specifies the package toolchain which also will be
  added to the package requires. If not set, the default is `system-native` if
  the package config name is `host` and `$jagen_target_toolchain` if the config
  name is `target`. The `false` value indicates that the package does not
  require a toolchain. Should be set to `false` for toolchain packages
  themselves to break dependency cycles.

- **build.type** (`pkg_build_type`) — The type of the package build system.
  Supported values are: gnu, cmake, kbuild, make, linux-kernel, linux-module.

- **build.unset_cflags** (`pkg_build_clean_cflags`) — _Deprecated_. An alias
  for **build.without_toolchain_cflags**.

- **build.with_install_dir** (`pkg_build_with_install_dir`) — If set,
  explicitly adds `-I${pkg_install_dir}/include` and `-L${pkg_install_dir}/lib`
  options to every toolchain invocation command when building this package.

  Some configure scripts try to link libraries by name (-lfoo) when probing
  dependencies and fail to find the libraries in the staging directory if no
  such library is installed on the host system or produce incorrect results by
  finding a different version. This option can be used to force such packages
  to look in the staging directory first.

- **build.without_toolchain_cflags** (`pkg_build_without_toolchain_cflags`) —
  If set to `true` causes the c/cxx/ld flags provided by a toolchain to not be
  added to every corresponding tool command line.

  Some (usually proprietary SDKs) packages are very sensitive to compiler
  settings and break in mysterious ways if compiled with anything different
  from their own hardcoded defaults. This option is useful for such cases.

The rules of the form:
```
build = { type = value }
```
can be shortened to:
```
build = value
```

## Install

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

- **install.type** (`pkg_install_type`) — Specifies the installation type of
  the package. If not set defaults to the value of `build.type`. An additional
  special type "toolchain" can be used to install toolchain wrappers.

The rules of the form:
```
install = { type = value }
```
can be shortened to:
```
install = value
```

## Global variables

- **jagen_FS** — Field separator (`<tab>`).
- **jagen_IFS** — IFS (`<newline><tab>`).
- **jagen_bin_dir** — The location of the `bin` directory
  (`$jagen_root_dir/bin`).
- **jagen_build_dir** — The location of the `build` directory
  (`$jagen_root_dir/build`).
- **jagen_build_profile** — Global package build profile.
- **jagen_build_verbose** — Indicates whether the build system should run
  verbosely.
- **jagen_ccache** — The name of the ccache executable.
- **jagen_cmake_executable** (`cmake`) — The executable to launch CMake.
- **jagen_cmake_generator** (`Ninja`) — The generator to use with CMake.
- **jagen_cmake_module_path** — The default `CMAKE_MODULE_PATH`.
- **jagen_cmake_options** — Additional arguments to pass to CMake in `configure` stage.
- **jagen_cmake_toolchain_file** — The `CMAKE_TOOLCHAIN_FILE` to pass to CMake
  in `configure` stage.
- **jagen_debug** — Debug level.
- **jagen_dir** — The location of the Jagen source directory.
- **jagen_dist_dir** — The location of the `dist` directory
  (`$jagen_root_dir/dist`).
- **jagen_flags** — Space-separated list of flags.
- **jagen_host_dir** — The location of the `host` directory
  (`$jagen_root_dir/host`).
- **jagen_include_dir** — The location of the `include` directory
  (`$jagen_root_dir/include`).
- **jagen_insecure** — Adds the `--insecure` flag to curl command during
  downloads if set. This disables verifying of servers certificates. Please be
  very careful with this flag! It is intended only for the case when a corporate
  proxy MITMs you with invalid certificate or build env does not have an
  appropriate CA installed but you are in a rush. Do not forget to unset this
  flag once the problem is fixed.
- **jagen_jobs** — The default number of jobs to run for build commands.
  Default: the number of processors on the system. Overriden by the
  package-specific `build.jobs` property.
- **jagen_layer_path** — The list of directories which are searched for
  unqualified layers.
- **jagen_layers** — The IFS-separated list of paths for used layers.
- **jagen_lib_dir** — The location of the `lib` dir (`$jagen_dir/lib`).
- **jagen_log_dir** — The location of the `log` dir (`$jagen_build_dir`).
- **jagen_lua** — The name of the Lua executable.
- **jagen_pager** — Overrides PAGER environment variable.
- **jagen_path** — Import path. Derived from layer locations.
- **jagen_private_dir** — The location of the private directory (can be set
  from project-specific config).
- **jagen_root_dir** — The location of the current project directory.
- **jagen_root_lib_dir** — The location of the current project `lib`
  directory (`$jagen_root_dir/lib`).
- **jagen_shell** — Override shell used to run internal scripts.
- **jagen_source_exclude** — A space-separated list of package name patterns
  which should not be updated or patched during the build. Applies only to
  source-based (SCM) packages, i.e. having source type "git", "hg" or "repo".
  In the patterns `*` matches zero or more characters, `?` matches any single
  character. If the pattern starts with `!` its meaning is negated, i.e. it
  excludes all not matching packages.
- **jagen_src_dir** — The location of the `src` directory
  (`$jagen_root_dir/src`).
- **jagen_target_board** — The name of the current target board.
- **jagen_target_dir** — The location of the `target` directory
  (`$jagen_root_dir/target`).
- **jagen_target_toolchain** — The name of the current target toolchain.

Settings **jagen_cmake_executable**, **jagen_cmake_generator**,
**jagen_cmake_module_path**, **jagen_cmake_options** and
**jagen_cmake_toolchain_file** have config-specific and toolchain-specific
veriants with higher priority, e.g. **toolchain_cmake_executable** overrides
**jagen_target_cmake_executable** which overrides **jagen_cmake_executable** if
set.

## Stage-specific variables

- **pkg_args** — stage arguments

- **pkg_build_cmake_executable** (`build.cmake_executable`) — the CMake
  executable use by configure stage.

- **pkg_build_cmake_generator** (`build.cmake_executable`) — the generator
  value passed to CMake during configure.

- **pkg_build_cmake_module_path** (`build.cmake_module_path`) — the
  `CMAKE_MODULE_PATH` for the package

- **pkg_build_cmake_options** (`build.cmake_options`) — additional
  CMake-specific options.

- **pkg_build_cmake_toolchain_file** (`build.cmake_toolchain_file`) —
  `CMAKE_TOOLCHAIN_FILE` for the current package

- **pkg_build_configure_file** (`build.configure_file`) — the path to the
  configure file; default: `$pkg_source_dir/configure`

- **pkg_build_dir** (`build.dir`) — the location of the package build directory

- **pkg_build_generator** (`build.generator`) — _Deprecated._ An alias for
  **pkg_build_cmake_generator**.

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
  package; currently supported values are: gnu, kbuild, cmake, make,
  linux-kernel, linux-module

- **pkg_config** (`pkg.config`) — the config of the currently executing stage

- **pkg_install_args** (`install.args`) — a list of additional install command
  arguments

- **pkg_install_config_script** (`install.config_script`) — specifies the
  location of the `*-config` script relative to the sysroot; some gnu packages
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

- **pkg_install_type** (`install.type`) — Specifies the installation type of
  the package. If not set defaults to the value of `build.type`. An additional
  special type "toolchain" can be used to install toolchain wrappers.

- **pkg_name** — the package name of the currently executing stage

- **pkg_run_on_error** — an error hook; obsolete, not used anymore

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

