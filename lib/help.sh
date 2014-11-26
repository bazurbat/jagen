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

  The build and rebuild commands accept target file names without directory.
  To see the full list use the command:

    ninja -f \"$pkg_build_dir/build.ninja\" -t targets all

  The target list is optional for the build command, if supplied only those
  targets will be built.

  The rebuild command have an optional arguments (in this order only):

    jagen rebuild [--targets-only] [--show-all] <TARGETS>...

  With the '--targets-only' option only the specified targets will be built
  (similar to the 'build' command).

  With the '--show-all' option all logs from the build directory will be shown
  instead of from the supplied targets only.

  The logs will not be shown if the targets were not previously built or the
  build directory was clean.

CONFIGURATION:

  User can supply additional configuration in \"~/.config/jagen/env.sh\" and
  \"<jagen>/local.sh\" files. These are sourced during the build process and
  should be the shell variable declarations, for example:

  export ja_sdk=\"sigma\"
  export pkg_build_dir=\"/tmp/build\"
  export pkg_source_exclude=\"chicken karaoke-player\"

NOTES:

  Targets with names ending with 'clean' usually wipe their respective package
  working and source directories. Add package names to the 'pkg_source_exclude'
  configuration variable as in the example above to stop the build scripts from
  touching their source directories (build directories will still be wiped).
"
}
