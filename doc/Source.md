# Managing package sources

```
Usage: jagen source <command> [PACKAGES...]

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

  Run `jagen source each --help` to see the reference for the 'each' subcommand.
```

It is possible to use `src` as an alternative command name instead of `source`.

## source each

Usage: `jagen source each <command>`

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

    jagen source each git status

  Run `ls` for all source directories:

    jagen source each ls

  Run `ls` only for "hg" sources:

    jagen source each --type hg ls

# Managing layers

```
Usage: jagen update [<LAYER>|jagen|self]...

  Updates the specified layers or Jagen itself.

SYNOPSIS

  Specify a list of shell-like patterns of layer names to update. To see all
  currently defined layers use the `jagen list layers` command. If nothing is
  specified then all layers will be updated.

  Special names 'jagen' and 'self' can be added to the list to also update the
  Jagen repository associated with the project. These special names do not
  participate in the layer name pattern matching and should be specified
  explicitly.

EXAMPLES

  To update all currently defined layers:

    jagen update

  To update only Jagen:

    jagen update self

  To update all layers and Jagen:

    jagen update self '*'

  To update only layers with names starting with 'ja':

    jagen update 'ja*'
```

Use the `update` command to update project layers or associated Jagen repo. Git
source type is assumed for both.
