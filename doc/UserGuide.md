# User Guide

_The guide is work in progress._

## Introduction

The workflow in Jagen is organized around projects (also called "build roots") which consist of a
number of "layers". Each layer can provide rules, configuration, "pkg" files, patches and other
resources which are used to generate a build system and environment of the project. This build
system tracks packages dependencies, allows rebuilding of the parts or the whole project and
managing of the source directories.

Typically, you have project's environment (`env.sh`) sourced in a terminal and do all actions
related to the project from this terminal window. This way you will have Shell command
auto-completion working properly, toolchains and tools compiled for the host in you PATH so you can
run them directly or perform some other actions manually. For non-interactive work (such as
launching the build from an IDE) there is a `jagen` script in each project's directory which
initializes the environment before redirecting the command to the Jagen's instance. Running this
script does not change the current Shell environment.

## Requirements

- [Lua](https://www.lua.org) 5.1 or 5.2 ([LuaJIT](http://luajit.org) 2.0 works as well)
- [Ninja build](https://ninja-build.org)
- [ccache](https://ccache.samba.org) is recommended if you plan to compile a lot of C/C++ code 

On Ubuntu Linux you can install the required packages using the command:

    sudo apt-get install lua5.1 ninja-build ccache

## Initialization

Use the `init` script in the root of Jagen source directory to initialize the current directory as
the project. It can be piped to Shell directly from GitHub:

    mkdir -p ~/src/jagen-project && cd ~/src/jagen-project
    curl -fsSL https://raw.githubusercontent.com/bazurbat/jagen/master/init | sh

You can pass parameters such as layers and flags to init using the form:

    curl ... | sh -s -- param1 param2

If you prefer to clone the repository manually to share the same Jagen instance between several
projects you can run `init` locally after the checkout. See [Init](Init.md) reference for more
details.

Add the provided Bash completions file `.jagen/misc/bash_completion` to your Shell completion
configuration. For one time trial just source this file in the current Shell.

    . .jagen/misc/bash_completion

After that source the generated `env.sh` file as well.

    . ./env.sh

Now the current Shell is initialized to work with the project and you can execute the `jagen`
command from anywhere.

## Adding Packages

The next step is to declare packages for the build system. The package declarations are found in
"rule files" called `rules.lua` across the "jagen path". The "jagen path" always contains the `lib`
directory of the current project as the last entry which means that it overrides the definitions
coming from layers if any.

Open the `lib/rules.lua` file.

```lua
package { 'nanomsg',
    source = 'https://github.com/nanomsg/nanomsg.git',
    build  = 'cmake'
}
```

This rule defines a package named "nanomsg" which should be downloaded from
https://github.com/nanomsg/nanomsg.git and built using CMake.

See more about writing rules in [Rules](Rules.md) reference.

## Building

Use `jagen build` command interact with the build system.

Running `jagen build` without any arguments ensures that all targets are up to date. When there is
nothing left to build you will see the:

    ninja: no work to do.

message from ninja. To rebuild a target specify it as the build command argument.

See more about building packages in [Build](Build.md) reference.

## Managing Sources

Use `jagen source` command to manage sources.

See more about managing package sources in [Source](Source.md) reference.

## Updating

Update all layers in the current projects using the `jagen update` command. Update the associated
Jagen repository using the `jagen update self` command.

See more about update command in [Update](Update.md) reference.
