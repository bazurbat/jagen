#!/bin/sh

print_help() {
    echo "Usage: $0 <COMMAND> [OPTIONS]...

Generates and manages a build system from predefined rules.

Commands:
  help		Print this help message
  clean		Delete the build directory ($pkg_build_dir)
  update	Update jagen and regenerate the build system
  refresh	Regenerate the build system
  build		Build the specified targets
  rebuild	Rebuild the specified targets showing the logs to stdout

  The build command accepts target file names without directory. To see the
  full list use the command:

    ninja -f \"$pkg_build_dir/build.ninja\" -t targets all

  The target list is optional command, if supplied only those targets will be
  built.

  The rebuild command have the form:

    jagen rebuild [-t|--targets-only] <TARGETS>...

  With the '-t' or '--targets-only' option only the specified targets will be
  built (similar to the 'build' command). In the default mode the rebuild will
  start from the specified targets and continue through their dependencies.
  
  The targets are specified as: <name>[:<stage>][:<config>] ...

  Components in square brackets are optional. If <stage> is empty - 'build' is
  assumed. If <config> is empty - all defined configs for the specified stage
  will be built. E.g.:
  
  utils
  utils:build
  utils:build:host utils:build:target

  are the same if the host and target configs are defined for the utils
  package. More examples:

  utils:install      - run all utils install stages
  utils:install:host - run only host utils install stage
  utils::host        - run only host utils build stage

CONFIGURATION:

  User can supply additional configuration in \"~/.config/jagen/env.sh\" and
  \"<jagen>/local.sh\" files. These are sourced during the build process and
  should be the shell variable declarations, for example:

  pkg_sdk=\"sigma\"
  pkg_build_dir=\"/tmp/build\"
  pkg_source_exclude=\"chicken karaoke-player\"

NOTES:

  Package stages with name 'clean' usually wipe their respective working and
  source directories. Add package names to the 'pkg_source_exclude'
  configuration variable as in the example above to stop the build scripts from
  touching their source directories (build directories will still be wiped).
"
}
