### Building

```
Usage: jagen build [OPTION...] [TARGET...]

  Builds or rebuilds the specified targets.

OPTIONS

  -h, --help          print this help message
  -n, --dry-run       print expanded value of TARGET... arguments and exit
  -p, --progress      show TARGET's build progress
  -P, --all-progress  show all build output
  -f, --force         force rebuild of the specified targets
  -a, --all           build everything out of date

  Use command 'jagen help targets' for information about targets.

SYNOPSIS

  If no targets were specified the command builds everything not already built;
  otherwise it expands TARGET... arguments and builds the resulting targets if
  they are out of date. The '--force' option causes the specified targets to be
  rebuilt unconditionally. Use '--all' option to also build dependencies with a
  single command.

  Short options can be combined into a single argument, for example:

    jagen build -fap libuv

  will rebuild all targets of libuv package (starting from 'unpack', see below)
  showing progress on the console. Afterwards it will continue building
  dependent targets until everything is up to date.

  Note that all packages start with 'unpack' stage which removes working
  directories from previous builds, cleans sources from extra files, unpacks
  distributions and updates the sources from scm. It does not touch scm sources
  if there are changes detected (jagen src dirty returns true) but still
  removes build directory if it is separate from source directory. This is
  especially important to remember when working on packages with "in source"
  builds.

  For development and testing it can be more convenient to select specific
  targets, like:

    jagen build -fp libuv:compile:target

  This will recompile libuv for target configuration showing output on the
  console and do nothing else.

```
