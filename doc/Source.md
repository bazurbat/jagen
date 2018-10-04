# Managing Sources

Jagen provides an abstraction on top of Git, Hg and Repo SCM systems to allow working with them
using the same set of commands.

## Rules

To enable source management facilities add `source` property to the package rule:

```lua
package { 'nanomsg',
    source = 'https://github.com/nanomsg/nanomsg.git'
}
```

This is a shorthand form for the following expanded rule:

```lua
package { 'nanomsg',
    source = {
        type = 'git',
        location = 'https://github.com/nanomsg/nanomsg.git'
    }
}
```

If the source type was not specified then Jagen will apply a heuristic algorithm to try to
determine it: if the location ending with well known archive extension then the type is set to
`dist`, if it is ending with ".git" or starting with "git@" or "https://github.com" or matching Git
style SSH URL then the type is `git`, if the location ends with ".hg" then the type is `hg`, if it
equals to "." then the type is `dir`, otherwise it is `dist`.

There is a shorthand form for overriding the type:
```lua
source = { 'git', 'https://some/repo/location' }
```

But, in general, if you want to override some settings or specify a branch or tag you will need to
use the expanded key-value form.

The source types `git`, `hg` and `repo` are considered "SCM" sources and can be managed by `source`
command. Other source types are handled implicitly during the "unpack" package build stage.

See [Source](Rules.md#source) section about rules for the list and description of all supported
properties. The `filename` and `basename` are set if the `location` is set. The `name` and `dir`
are always set.

Note that setting a revision or tag for Git source makes the working directory "detached" so it is
fixed on a given reference. If a branch is specified Jagen will check it out locally if not already
exists (using the default behaviour of `checkout` command) and try to follow by merging its remote
with fast-forward on each update. This mode can also be convenient for repositories which are
seldom worked on to allow making commits and pushing from the working directory the same way as
when using Git manually.

As a safety measure, during the update stage or with the `jagen source update` command Jagen will
refuse to update repositories which have unsaved changes (dirty). Clean them or stash the changes
to let the update continue.

For packages under an active development, when you want to have a local checkout of a branch which
does not have the corresponding remote yet, or want to have complete control over the repository
sources set the `exclude` property to `true`. This will make the default unpack and patch stages
implementations skip the source directory during the build. This exclusion does not apply to `jagen
source` command.

## Commands

```
jagen source <command> [PACKAGES...]
```

The optional `PACKAGES` argument should be a list of SCM packages defined in the current
environment. If nothing is specified then the command will be applied to all SCM packages.

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
