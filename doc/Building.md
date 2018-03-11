## Building

Usage: jagen build [OPTION...] [PATTERN...]

  Builds or rebuilds the specified targets.

### Options

```
  -h, --help          print this help message
  -m, --match         print expanded value of target patterns and exit
  -c, --clean         clean package's build directories before the build
  -a, --all           continue until everything is up to date
  -n, --no-rebuild    do not rebuild targets which are already up to date
  -p, --progress      show build progress
  -P, --all-progress  show all build output
```

  Use the command 'jagen help targets' for information about targets.

### Synopsis

  The specified patterns are expanded and matching targets are rebuilt. Use the
  '--match' option to print the matches without building anything.

  Use the '--clean' option to remove the package's build directories before the
  start. It also causes the 'configure' stage of the affected packages to
  become out of date.

  Use the '--all' option to build everything out of date in the current project
  in addition to the specified targets.

  The '--no-rebuild' option causes the command to behave similarly to 'make':
  it ensures that targets are up to date instead of rebuilding them
  unconditionally.

  The '--progress' option enables the printing of the build progress for the
  specified targets, the '--all-progress' option prints all build output
  instead.
