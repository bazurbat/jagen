# Managing package sources

```
Usage: jagen src <command> [PACKAGES...]

  Manage SCM package sources.

SYNOPSIS

  The optional PACKAGES argument should be the list of SCM packages defined in
  the current environment. If none are specified, all are assumed.

COMMANDS

  dirty   Check if packages source directories have any changes
  status  Show packages location, head commit and dirty status
  clean   Clean up packages source directories
  update  Update the sources to the latest upstream version
  clone   Clone the specified packages
  delete  Delete packages source directories
  each    Execute Shell command inside each source directory

  The 'dirty' command exits with 0 (true) status if any of the specified
  packages source directories have changes. It exits with 1 (false) status if
  all sources are clean. Intended for usage by shell scripts.

  The 'status' command prints SCM packages status in human readable form.

  The 'clean' command resets modifications to the HEAD state and deletes
  all extra files in packages source directories.

  The 'update' command fetches the latest sources from upstream and tries to
  merge them. It does nothing if there are modifications in the working
  directory (dirty returns true); commit, stash or clean changes before issuing
  the 'update' command in this case.

  The 'clone' command clones the specified packages.

  The 'delete' command deletes packages source directories.

  Run `jagen src each --help` to see the reference for the 'each' subcommand.
```

## src each

Usage: `jagen src each <command>`

Execute Shell command for each source directory.

### Synopsis

The `each` subcommand concatenates its arguments and executes the result as
Shell command inside the source directories. Use the `--type` argument to
filter the source directories by type, e.g. `--type git` will run the command
only for Git sources. If the type is not specified but the command starts
with one of the known source types (git, hg and repo) then it will be set
implicitly, otherwise the command will be run for all source directories.

### Examples

  Run `git status` for sources of type "git":

    jagen src each git status

  Run `ls` for all source directories:

    jagen src each ls

  Run `ls` only for "hg" sources:

    jagen src each --type hg ls
