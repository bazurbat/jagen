local usage = [[
Usage: jagen <COMMAND> [OPTIONS...]

  Generates and manages a build system according to the predefined rules.

Commands:
  help		Print this help message
  clean		Delete the build directory
  update	Update jagen and regenerate the build system
  refresh	Regenerate the build system
  run   	Run the build system for the specified targets
  src		Manage SCM package sources

  The 'help' command shows this help message.

  The 'clean' command removes the build directory and regenerates the build
  system. Currently configured build directory:

    $jagen_build_dir

  The 'update' command tries to update jagen from the source repository and
  regenerates the build system afterwards.

  The 'refresh' command generates build system from rules according to the
  configuration.

  The 'run' command have the form:

    jagen run [-t] [-o] [-a] [TARGETS...]

  and runs the the build system starting from the specified targets. Add the
  '-t' option to stop after given targets are completed. With the '-o' option
  the command output will be shown for the specified targets. With the '-a'
  option all build output will be shown.

  The targets are specified as: <name>[:<stage>][:<config>]. The available
  package stages are filtered with the given expression. Omitted component
  means 'all'. For example:

  utils              - select all stages of the utils package
  utils:install      - select all utils install stages
  utils::host        - select all host utils stages
  utils:build:host   - select only host utils build stage
  :build:host        - select host build stages of all packages

  When a target is succesfully built the stamp file is created in the build
  directory with the name: <name>-<stage>-<config>. This file is used to
  determine if the target is up to date. Deleting it will cause the
  corresponding target to be rebuilt unconditionally next time the build system
  runs.

CONFIGURATION:

  User can supply additional configuration in \"<jagen_root>/config.sh\" file.
  The configuration template is generated by init-root script. See its contents
  for additional information.

NOTES:

  Package stages with name 'clean' usually wipe their respective working and
  source directories. Add package names to the 'jagen_source_exclude'
  configuration variable as in the example above to stop the build scripts from
  touching their source directories (build directories will still be wiped).

]]

local src = [[
Usage: jagen src <subcommand> [PACKAGES...]

  Manage SCM package sources.

  The optional PACKAGES argument should be the list of SCM packages defined in
  the current environment. If none are specified, all are assumed.

Available subcommands:

    dirty   Check if packages source directories have any changes
    status  Show packages location, head commit and dirty status
    clean   Clean up packages source directories
    update  Update the sources to the latest upstream version
    clone   Clone the specified packages
    delete  Delete packages source directories

  The 'dirty' subcommand exits with 0 (true) status if any of the specified
  packages source directories have changes. It exits with 1 (false) status if
  all sources are clean. Intended for usage by shell scripts.

  The 'status' subcommand prints SCM packages status in human readable form.

  The 'clean' subcommand resets modifications to the HEAD state and deletes
  all extra files in packages source directories.

  The 'update' subcommand fetches the latest sources from upstream and tries
  to merge them with the current source directories.

  The 'clone' subcommand clones the specified packages.

  The 'delete' subcommand deletes packages source directories.

]]

return {
    usage = usage,
    src = src
}