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

In short, it's fresh remix of ideas from [Repo][], [BitBake][] and
[Buildroot][].

  [Repo]: https://source.android.com/source/using-repo.html
  [BitBake]: https://en.wikipedia.org/wiki/BitBake
  [Buildroot]: https://buildroot.org

## Requirements

POSIX compatible shell, Lua 5.1 or 5.2, [Ninja](https://ninja-build.org/).

## Getting Started

```
cd ~/src
git clone https://github.com/bazurbat/jagen.git
mkdir root-genivi
cd root-genivi
../jagen/init-project vendor/genivi
. ./env.sh
jagen build
```

the sources are checked out to the `src` subdirectory. Working directory for
packages is in `build/<name>`.

```
vim src/AudioManager/AudioManagerDaemon/src/main.cpp
jagen build -fa audio-manager:compile:host
```

resume work:

```
cd ~/src/root-genivi
. ./env.sh
jagen build
```

See [Manual](doc/Manual.md) for more details.
