# Managing Sources

Jagen provides uniform way to manage Git and Hg packages sources.

```
jagen source <command> [PACKAGES...]
```

The optional `PACKAGES` argument should be a list of SCM packages defined in the current
environment. If nothing is specified then all SCM packages from the current project will be used.

## Commands

Command | Description
--------|------------
status  | Show packages location, head commit and dirty status
update  | Update the sources to the latest upstream version
clean   | Clean up packages source directories
delete  | Delete packages source directories
dirty   | Check if packages source directories have any changes
each    | Execute Shell command inside each source directory

The `status` command prints SCM packages status in human readable form.

The `update` command fetches the latest sources from upstream and tries to merge them. It does
nothing if there are modifications in the working directory (dirty returns true); commit, stash or
clean changes before issuing the 'update' command in this case.

The 'clean' command resets modifications to the HEAD state and deletes all extra files in packages
source directories.

The 'delete' command deletes packages source directories.

The 'dirty' command exits with 0 (true) status if any of the specified packages source directories
have changes. It exits with 1 (false) status if all sources are clean. Intended for usage by shell
scripts.

It is possible to use `src` as an alternative command name instead of `source`.

## each

```
jagen source each <command>
```

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
