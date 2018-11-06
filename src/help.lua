local usage = [[
Usage: jagen <COMMAND> [OPTIONS...]

  Generates and manages a build system according to the predefined rules.

COMMANDS

  help      Show jagen usage information
  clean     Clean up build root
  refresh   Regenerate the build system
  build     Build or rebuild the specified targets
  source    Manage SCM package sources
  list      List various information about the current project
  update    Update the specified layers or Jagen itself

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
Usage: jagen build [OPTION...] [PATTERN...] [--] [TOOL OPTIONS...]

  Builds or rebuilds the specified targets.

OPTIONS

  -h, --help          print this help message
  -m, --match         print expanded value of target patterns and exit
  -c, --clean         clean package's build directories before the build
  -i, --interactive   enable interactive mode
  -a, --all           continue until everything is up to date
  -n, --no-rebuild    do not rebuild targets which are already up to date
  -f, --follow        follow a build output for the specified targets only
  -F, --follow-all    follow all build output
  -q, --quiet         inhibit build output

  Use the command 'jagen help targets' for information about targets.

SYNOPSIS

  The specified patterns are expanded and matching targets are rebuilt. Use the
  '--match' option to print the matches without building anything.

  Use the '--clean' option to remove the package's build directories before the
  start. It also causes the 'configure' stage of the affected packages to
  become out of date.

  Use the '--interactive' option to allow build tools to detect the terminal and
  show colored messages. This mode ignores target dependencies and does not
  capture command output. Run build without '-i' to bring all targets up to date
  before using this option.

  Use the '--all' option to build everything out of date in the current project
  in addition to the specified targets.

  The '--no-rebuild' option causes the command to behave similarly to 'make':
  it ensures that targets are up to date instead of rebuilding them
  unconditionally.

  The '--follow' option allows monitoring the output from build commands in real
  time. It shows the output only for targets specified for the current build
  command. Works best when used for a single package or dependent targets
  because when there are several package builds in progress their output will be
  intermixed.

  The '--follow-all' option shows the output from all currently running build
  commands at the same time. This includes the targets building as dependencies
  of the ones specified as build command arguments and all others not currently
  up to date if used in combination with '--all' option.

  If neither the '--follow' nor the '--follow-all' option is given the output of
  all commands shown in turn after they complete.

  The '--quiet' option disables printing of the command outputs to the terminal.
  The output is still saved to logs.

  Arguments after '--' will be passed literally to the underlying build tool.
  The handling depends on the build tool in question but be aware of the case
  when multiple targets matched as the same arguments will be passed to all of
  their build commands which might have surprising results.

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

local source = [[
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

]]

local src_each = [[
Usage: jagen source each <command>

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

    jagen source each git status

  Run `ls` for all source directories:

    jagen source each ls

  Run `ls` only for 'hg' sources:

    jagen source each --type hg ls

]]

local image = [[
Usage: jagen image <command> <options...>

  Creates and manages filesystem images.

COMMANDS

  Subcommands are project-specific.

]]

local list = [[
Usage: jagen list [packages|layers] [OPTIONS...]

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

  layers  Show currently defined layers and their file paths.

]]

local update = [[
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

]]

return {
    usage   = usage,
    help    = help,
    clean   = clean,
    refresh = refresh,
    build   = build,
    source  = source,
    src     = source,
    src_each = src_each,
    image   = image,
    list    = list,
    update  = update,
    targets = targets
}
