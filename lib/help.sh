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

  The 'help' command show this help message.

  The 'clean' command removes build directory.

  The 'update' command tries to update jagen from the source repository and
  does refresh afterwards.

  The 'refresh' command generates build system from rules according to the
  configuration.

  The 'build' command have the form:

    jagen build [TARGETS...]

  see below for TARGETS syntax.

  The 'rebuild' command have the form:

    jagen rebuild [-t] [-a] [TARGETS...]

  With the '-t' option only the specified targets will be rebuilt. In the
  default mode the rebuild will start from the specified targets and continue
  through their dependencies. The '-a' options shows all build logs, even from
  not specified targets (build dependencies for example).
  
  The targets are specified as: <name>[:<stage>][:<config>]. The available
  package stages are filtered with the given expression. Omitted components
  means 'all'. For example:

  utils              - run all stages of the utils package
  utils:install      - run all utils install stages
  utils::host        - run all host utils stages
  utils:build:host   - run only host utils build stage

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
