local usage = [[
Usage: jagen <COMMAND> [OPTIONS...]

  Generates and manages a build system according to the predefined rules.

COMMANDS

  help      Show jagen usage information
  clean     Clean up build root
  refresh   Regenerate the build system
  build     Build or rebuild the specified targets
  src       Manage SCM package sources
  show      Display build logs

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
  merge them. It does nothing if there are modifications in the working
  directory (dirty returns true); commit, stash or clean changes before issuing
  the 'update' command in this case.

  The 'clone' command clones the specified packages.

  The 'delete' command deletes packages source directories.

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

local show = [[
Usage: jagen show <target>

  Displays build logs.

SYNOPSIS

  Displays the log file for the specified target if it exists. Uses PAGER
  environment variable or 'less' if not defined. Use 'jagen_pager' to override.

]]

return {
    usage   = usage,
    help    = help,
    clean   = clean,
    refresh = refresh,
    build   = build,
    src     = src,
    image   = image,
    show    = show,
    targets = targets
}
