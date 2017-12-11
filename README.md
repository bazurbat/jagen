# Jagen

Jagen is a tool which eases development of multiple interdependent software
packages. It abstracts peculiarities of individual packages build systems and
provides means to manage them as a whole.

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

## Requirements

POSIX compatible shell, Lua 5.1 or 5.2, [Ninja](https://ninja-build.org/).

## Getting Started

```
cd ~/src
git clone https://github.com/bazurbat/jagen.git   # 1
mkdir root-genivi                                 # 2
cd root-genivi
../jagen/init-project genivi -a ccache            # 3
. ./env.sh                                        # 4
jagen build                                       # 5
```

1. Clone the Jagen itself into a `~/src/jagen` directory.
2. Create the `root-genivi` directory alongside (`~/src/root-genivi`) which
   will become the root of the project.
3. Initialize the current directory as a project. The name "genivi" is one of
   the products distributed with Jagen. You can also specify a list of layer
   paths or "." which means "nothing" (empty project).
   The "-a" option enables the "ccache" for the project.
4. Source the `env.sh` to initialize the project environment. Repeat this step
   each time you want to resume working with this project (i.e. to be able to
   run `jagen` command).
5. Build the project. The `build` command runs to completion, i.e. it ensures
   that all default targets are built, if it already the case — it does nothing
   (just as "make" for example).

The source repositories will be cloned to the `src` directory during the build.
Inspect what is currently checked out with the
```
jagen src status
```
command.

The distribution archives are downloaded to the `dist` directory.

You can find working directories along with build output files for each package
and stage in the `build` directory of the project.

The results are installed to the `host` subdirectory.

Use the
```
jagen list packages
```
command to see which packages compose the current project and where to find
their definitions. Inspect the referred files for examples on how to write
custom rules.

You can start by adding project-specific rules to `lib/rules.lua` file. For
example, add the following to the `~/src/root-genivi/lib/rules.lua`:
```lua
package { 'nanomsg', 'host',
    source = {
        type = 'git',
        location = 'https://github.com/nanomsg/nanomsg.git',
        branch = '1.1.2'
    },
    build = { type = 'CMake' }
}
```
Run the build again:
```
jagen build
```
You will see the `nanomsg` downloaded to the `src/nanomsg`, built in the
`build/nanomsg/host` and installed to `host` subdirectory of the current
project.

Add your own project in a similar fashion. If you have an existing project and
do not want to download anything just place it in the `src` directory and omit
the `source` part of the rule:
```lua
package { 'nanomsg', 'host',
    build = { type = 'CMake' }
}
```

Use the `jagen help` command or `--help` argument to the mentioned commands to
get more information about their possible options.

Do not forget to add `jagen/misc/bash_completion` to your Bash completion
configuration to make overall experience much more pleasant.

See [Manual](doc/Manual.md) for reference.
