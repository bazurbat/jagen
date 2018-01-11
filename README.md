# Jagen

Jagen eases the development of multiple interdependent software packages by
generating a meta project which abstracts the peculiarities of their respective
build and source control systems and provides instruments to uniformly manage
them as a whole.

It mainly targets embedded development use cases when things should be done
"your way" instead of shoving the requirements into rigid paradigms of Yocto
and Buildroot. In particular, integrating third-party SDKs, pre-built
toolchains or packages with non-standard build systems intended to be a
straightforward process. The focus is on ease of use and adoption rather than
trying to provide yet another idiosyncratic collection of recipes for every
possible piece of software. On the other hand, vendor and project-specific
recipe libraries and patches are well-supported and adding packages using
mainstream build systems (such as Autotools and CMake) is trivial.

The generated build system is simple to understand and customize. It collects
all commands outputs and allows to selectively repeat any stage (such as
configure or compile) of a single or multiple packages respecting the
dependencies and running to completion of a certain target such as creation of
a filesystem image. It is possible to combine multiple projects or parts into a
single product using the concept of "layers".

The built-in workspace management facilities allow SCM-controlled (Git and
Mercurial) packages to be inspected, updated, cleaned up, etc. using universal
commands. They replicate the most common use cases of Google's Repo and gclient
tools such as cloning and updating of multiple repositories. The sources in the
form of distribution archives are also supported. They will be automatically
downloaded and verified during the build. The source repositories and archives
are cached and can be shared between different projects.

Jagen is a mature project which is used in production since 2014.

## Requirements

POSIX compatible shell, Lua 5.1 or 5.2 (or LuaJIT 2.0), [Ninja](https://ninja-build.org/).

## Getting Started

To start using Jagen execute the following commands:

```
cd ~/src                                          # 1
git clone https://github.com/bazurbat/jagen.git   # 2
mkdir root-genivi                                 # 3
cd root-genivi                                    # 4
../jagen/init-project genivi -a ccache            # 5
. ./env.sh                                        # 6
jagen build                                       # 7
```

1. Go to the work directory. Can be anything but we assume `~/src`.
2. Clone the Jagen itself into the `~/src/jagen` directory.
3. Create the `root-genivi` (`~/src/root-genivi`) directory alongside the Jagen
   clone which will become the root of the project.
4. Go to the just created directory.
5. Initialize the this directory as a project. The name "genivi" is one of the
   project templates distributed with Jagen. The "-a" option enables the
   "ccache" for the project.
6. Source the generated `env.sh` file to initialize the project environment.
   Repeat starting from this step each time you want to resume working with
   this project (i.e. to be able to run `jagen` command).
7. Build the project. The `build` command runs to completion, i.e. it ensures
   that all default targets are built, if it already the case — it does nothing
   (just as "make" for example).

See [Initializing](doc/Initializing.md) section in the manual for more details.

During the build the source repositories will be cloned to the `src` directory,
the distribution archives will be downloaded to the `dist` directory. Inspect
what is currently checked out with the `jagen src status` command.

You can find working directories along with build output files for each package
and stage in the `build` directory.

The results are installed to the `host` directory.

Use the `jagen list packages` command to see which packages compose the current
project and where to find their definitions. Inspect the referred files for
examples on how to write custom rules.

### Custom rules

Add project-specific rules to the `lib/rules.lua` file inside the project
directory. For example, write the following:

```lua
package { 'nanomsg', 'host',
    source = {
        type = 'git',
        location = 'https://github.com/nanomsg/nanomsg.git',
        branch = '1.1.2'
    },
    build = {
        type = 'CMake'
    }
}
```

And run the build again: `jagen build`.

You will see the `nanomsg` downloaded to the `src/nanomsg`, built in the
`build/nanomsg/host` and installed to the `host` subdirectory of the project.

### External project

If you want to use Jagen to manage an existing project add the following to the
`lib/rules.lua`:

```lua
package { 'myproject', 'host',
    source = {
        dir = '$HOME/src/myproject'
    },
    build = {
        type = 'CMake'
    },
    install = {
        type = 'none'
    }
}
```

Assuming it uses `CMake` as the build system. Note that you can use shell
variables in the values. Paths with spaces are also OK. Install 'none' means do
not install it to the `host` directory.

Run `jagen build` again to build the just added package. If some stage failed,
note the message similar to:
```
FAILED: jagen-pkg myproject compile host
```
and look to the `build/myproject__compile__host.log` for the build output of
this stage.

Once everything is built you can rebuild a single stage using the command:
`jagen build -fp myproject:compile:host`. Use the `-p` option to watch the
build output. See [Building](doc/Building.md) for more details.

Use the `jagen help` command or `--help` argument to the mentioned commands to
get more information about their possible options.

Do not forget to add `jagen/misc/bash_completion` to your Bash completion
configuration to make overall experience much more pleasant.

See [Manual](doc/Manual.md) for reference.
