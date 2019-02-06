### Cleaning

```
Usage: jagen clean [package[:config]...]

  Deletes package build directories or all generated files and directories
  inside the current build root.

OPTIONS

  -y, --ignore-dirty    ignore dirty status of source directories
  -x, --ignore-exclude  do not skip excluded packages

SYNOPSIS

  There can be multiple arguments in the form of: <name> or <name>:<config>.
  Build directories of given packages will be removed. If <config> is supplied
  the corresponding build directory will be removed, if only <name> is supplied
  all build directories will be removed.

  If no arguments are given, than everything in the current build root is
  cleaned up. The following directories are recursively deleted:

    jagen_build_dir
    jagen_include_dir
    jagen_log_dir
    jagen_host_dir
    jagen_target_dir

  Actual paths depend on configuration. After the deletion regenerates the
  build system using the 'jagen refresh' command.
```
