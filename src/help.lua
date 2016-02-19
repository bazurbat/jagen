local usage = [[
Usage: jagen <COMMAND> [OPTIONS...]

  Generates and manages a build system according to the predefined rules.

COMMANDS

  help      Show jagen usage information
  clean     Clean up build root
  refresh   Regenerate the build system
  build     Ensure that targets are up to date
  rebuild   Rebuild targets with dependencies
  src       Manage SCM package sources

  Use 'jagen help <command>' to get help about individual commands.

]]

local help = [[
Usage: jagen help [section]
       jagen --help [section]
       jagen <section> help
       jagen <section> --help

  Shows usage information about jagen commands or other help sections.

SYNOPSIS

  If a command was specified before or after help argument its usage will be
  shown, if there are other help section available with the given name - this
  section will be shown, or general usage information will be shown if nothing
  was specified.

  It is also possible to use '-h' instead of '--help'.

]]

local clean = [[
Usage: jagen clean

  Deletes all generated files and directories inside the current build root.

SYNOPSIS

  The following directories are recursively deleted:

    jagen_build_dir
    jagen_include_dir
    jagen_log_dir
    jagen_host_dir
    jagen_target_dir
    jagen_tools_dir

  Actual paths depend on configuration. After the deletion regenerates the
  build system using the 'jagen refresh' command.

]]

local refresh = [[
Usage: jagen refresh

  Regenerates the build system from rules according to configuration.

]]

local build = [[
Usage: jagen build [OPTION...] [TARGET...]

  Builds the specified targets or everything out of date.

OPTIONS

  -n  Print expanded value of TARGET... arguments and exit.
  -p  Show build progress for the specified targets.
  -P  Show all build output.

  Use command 'jagen help targets' for information about targets.

SYNOPSIS

  If no targets were specified it builds everything not already built;
  otherwise it expands TARGET... arguments and builds the resulting targets if
  they are out of date.

]]

local rebuild = [[
Usage: jagen rebuild [OPTIONS...] [TARGETS...]

  Rebuilds the specified targets and optionally their dependencies.

OPTIONS

  -n  Print expanded value of TARGET... arguments and exit.
  -p  Show build progress for the specified targets.
  -P  Show all build output.
  -a  Also build dependencies.

  Use command 'jagen help targets' for information about targets.

SYNOPSIS

  If no targets were specified it builds everything not already built;
  otherwise it expands TARGET... arguments and rebuilds the resulting targets
  unconditionally. Use the '-a' option to continue until everything is up to
  date (build all).

]]

local targets = [[

  Targets are specified as '<name>:<stage>:<config>'. Available package stages
  are filtered with the given expression. Omitted component means 'all'.  For
  example:

  utils              - select all stages of the utils package
  utils:install      - select all utils install stages
  utils::host        - select all host utils stages
  utils:compile:host - select only host utils compile stage
  :build:host        - select host build stages of all packages

  When a target is succesfully built the stamp file is created in the build
  directory with the name: <name>__<stage>__<config>. This file is used to
  determine if the target is up to date. Deleting it will cause the
  corresponding target to be rebuilt unconditionally next time the build system
  runs.

]]

local src = [[
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

  The 'dirty' command exits with 0 (true) status if any of the specified
  packages source directories have changes. It exits with 1 (false) status if
  all sources are clean. Intended for usage by shell scripts.

  The 'status' command prints SCM packages status in human readable form.

  The 'clean' command resets modifications to the HEAD state and deletes
  all extra files in packages source directories.

  The 'update' command fetches the latest sources from upstream and tries to
  merge them with the current source directories.

  The 'clone' command clones the specified packages.

  The 'delete' command deletes packages source directories.

]]

return {
    usage   = usage,
    help    = help,
    clean   = clean,
    refresh = refresh,
    build   = build,
    rebuild = rebuild,
    src     = src,
    targets = targets
}
