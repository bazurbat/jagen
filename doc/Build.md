## Building

`jagen build [OPTION...] [PATTERN...] [--] [TOOL OPTIONS...]`

Builds or rebuilds the specified targets.

### Options

Option               | Description
---------------------|------------
-h, --help           | print this help message
-m, --match          | print expanded value of target patterns and exit
-c, --clean          | clean package's build directories before the build
-a, --all            | continue until everything is up to date
-n, --no-rebuild     | do not rebuild targets which are already up to date
-p, --progress       | print the output of build targets after completion
-f, --follow         | follow a build output for the specified targets only
-F, --follow-all     | follow all build output
-q, --quiet          | inhibit build output
-y, --ignore-dirty   | ignore dirty status of source directories
-x, --ignore-exclude | do not skip excluded packages

### Synopsis

The specified patterns are expanded and matching targets are rebuilt. Use the
'--match' option to print the matches without building anything.

Use the '--clean' option to remove the package's build directories before the
start. It also causes the 'configure' stage of the affected packages to become
out of date.

Use the '--all' option to build everything out of date in the current project in
addition to the specified targets.

The '--no-rebuild' option causes the command to behave similarly to 'make': it
ensures that targets are up to date instead of rebuilding them unconditionally.

The '--progress' option prints the buffered output of all build targets as soon
as they complete. 

The '--follow' option allows monitoring the output from build commands in real
time. It shows the output only for targets specified for the current build
command. Works best when used for a single package or dependent targets because
when there are several package builds in progress their output will be
intermixed.

The '--follow-all' option shows the output from all currently running build
commands at the same time. This includes the targets building as dependencies of
the ones specified as build command arguments and all others not currently up to
date if used in combination with '--all' option.

With the '--quiet' option the output from commands is collected to logs but not
displayed on the console during the execution unless the stage finished with an
error.

Arguments after '--' will be passed literally to the underlying build tool.  The
handling depends on the build tool in question but be aware of the case when
multiple targets matched as the same arguments will be passed to all of their
build commands which might have surprising results.

It is best to use this functionality to quickly adjust the behaviour of a
single compile or configure command, such as:

    jagen build nanomsg:compile:host -- -v

to pass `-v` flag to Ninja to temporarily enable verbose mode, or, in the case
of 'make' build type:

    jagen build someppkg:compile:host -- V=1

Another possibility:

    jagen build nanomsg:configure:host -- -DSOME_FLAG=ON

to enable CMake flag `SOME_FLAG` without going to the build directory.
