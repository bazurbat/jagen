local usage = [[
Usage: jagen <COMMAND> [OPTIONS...]

  Generates and manages a build system according to the predefined rules.

COMMANDS

  help      Show jagen usage information
  clean     Clean up build root
  refresh   Regenerate the build system
  build     Build or rebuild the specified targets
  src       Manage SCM package sources
  list      List various information about the current project

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
Usage: jagen clean [package[:config]...]

  Deletes package build directories or all generated files and directories
  inside the current build root.

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

]]

local refresh = [[
Usage: jagen refresh

  Regenerates the build system from rules according to configuration.

]]

local build = [[
Usage: jagen build [OPTION...] [PATTERN...]

  Builds or rebuilds the specified targets.

OPTIONS

  -h, --help          print this help message
  -m, --match         print expanded value of target patterns and exit
  -c, --clean         clean package's build directories before the build
  -a, --all           continue until everything is up to date
  -n, --no-rebuild    do not rebuild targets which are already up to date
  -p, --progress      show build progress
  -P, --all-progress  show all build output

  Use the command 'jagen help targets' for information about targets.

SYNOPSIS

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

]]

local src_each = [[
Usage: jagen src each <command>

  Execute Shell command for each source directory.

SYNOPSIS

  The 'each' subcommand concatenates its arguments and executes the result as
  Shell command inside the source directories. Use the '--type' argument to
  filter the source directories by type, e.g. '--type git' will run the command
  only for 'git' sources. If the type is not specified but the command starts
  with one of the known source types (git, hg and repo) then it will be set
  implicitly, otherwise the command will be run for all source directories.

EXAMPLES

  Run `git status` for sources of type 'git':

    jagen src each git status

  Run `ls` for all source directories:

    jagen src each ls

  Run `ls` only for 'hg' sources:

    jagen src each --type hg ls

]]

local image = [[
Usage: jagen image <command> <options...>

  Manages filesystem images.

COMMANDS

  create  Create images

  The 'create' command creates filesystem images according to configuration.
  Currently the only possible command is:

    jagen image create rootfs

  in 'hi-linux' build root which creates rootfs image.

]]

local list = [[
Usage: jagen list packages [OPTIONS...]

  Lists various information about the current project.

COMMANDS

  packages  List package rules and their origin

  The 'packages' command displays a list of all currently defined packages
  along with contexts where their definitions were found. Contexts could be
  rule files or other packages which mention given package as their requires.
  In the displayed filenames the parent directory of the current project is
  shown as '...'.

  packages options:

    --all, -a
        Show also implicit rules added by the generator such as the toolchain
        dependencies. These rules will be marked with "*".

    --depth,-d <level>
        Set the maximum depth of the rule contexts displayed. If none was
        specified the 0 is the default which results in showing only the
        toplevel packages explicitly defined in the rule files. If the option
        is given without a value it is set to 999 which means show all
        contexts.

]]

return {
    usage   = usage,
    help    = help,
    clean   = clean,
    refresh = refresh,
    build   = build,
    src     = src,
    src_each = src_each,
    image   = image,
    list    = list,
    targets = targets
}
