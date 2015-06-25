#!/bin/sh

print_help() {
    echo "Usage: jagen <COMMAND> [OPTIONS...]

  Generates and manages a build system according to the predefined rules.

Commands:
  help		Print this help message
  clean		Delete the build directory
  update	Update jagen and regenerate the build system
  refresh	Regenerate the build system
  build		Build the specified targets
  rebuild	Rebuild the specified targets showing the output

  The 'help' command shows this help message.

  The 'clean' command removes the build directory and regenerates the build
  system. Currently configured build directory:

    $pkg_build_dir

  The 'update' command tries to update jagen from the source repository and
  regenerates the build system afterwards.

  The 'refresh' command generates build system from rules according to the
  configuration.

  The 'build' command have the form:

    jagen build [TARGETS...]

  and builds the specified targets which are not up to date. If no targets are
  specified then all will be checked. Build output for each target is saved in
  the build directory alongside the target stamp file with the '.log' extension
  appended. See below for TARGETS syntax.

  The 'rebuild' command have the form:

    jagen rebuild [-t] [-a] [TARGETS...]

  and rebuilds the specified targets including their dependencies showing the
  output on the console. With the '-t' option only the specified targets will
  be rebuilt. With the '-a' option the console output will include logs from
  not specified targets. The output is saved alongside the target stamp file in
  the build directory with the '.log' extension appended.

TARGETS:

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
  corresponding target to be rebuilt unconditionally on the next 'build' or
  'rebuild' command.

CONFIGURATION:

  User can supply additional configuration in \"\$XDG_CONFIG_HOME/jagen/env\"
  or \"\$HOME/.config/jagen/env\" and \"<jagen_root>/local.sh\" files. These
  are sourced during the build process and should have the form of shell
  variable declarations, for example:

  pkg_sdk=\"sigma\"
  pkg_build_dir=\"/tmp/build\"
  pkg_source_exclude=\"chicken karaoke-player\"

  In fact, any shell code is possible, but it is advised to stick to POSIX
  shell syntax for portability.

NOTES:

  Package stages with name 'clean' usually wipe their respective working and
  source directories. Add package names to the 'pkg_source_exclude'
  configuration variable as in the example above to stop the build scripts from
  touching their source directories (build directories will still be wiped).
"
}
